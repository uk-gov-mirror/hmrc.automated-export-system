/*
 * Copyright 2026 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.automatedexportsystem.services

import cats.data.EitherT
import uk.gov.hmrc.automatedexportsystem.errors.{AesErrorMapper, MongoError, SubmissionServiceError}
import uk.gov.hmrc.automatedexportsystem.models.IE507.aes.{AesIE507Message, SubmissionId}
import uk.gov.hmrc.automatedexportsystem.models.IE507.{EoriNumber, ExportOperationType, Mrn}
import uk.gov.hmrc.automatedexportsystem.models.http.HttpHeader
import uk.gov.hmrc.automatedexportsystem.models.mongo.SingleUpdateStatus
import uk.gov.hmrc.automatedexportsystem.models.mongo.write.MongoAesIE507Message
import uk.gov.hmrc.automatedexportsystem.models.notification.{AesDigitalNotification, NotificationEventStatus, NotificationStatus}
import uk.gov.hmrc.automatedexportsystem.models.responses.{Submission, SubmissionSummary, SubmissionSummaryList}
import uk.gov.hmrc.automatedexportsystem.repositories.AesIE507Repository

import java.time.{Clock, Instant}
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

trait SubmissionService:
  def submitMessage(
    message:             AesIE507Message,
    exportOperationType: ExportOperationType,
    eoriNumber:          EoriNumber,
    maybeCorrelationId:  Option[HttpHeader.CorrelationId]
  ): EitherT[Future, SubmissionServiceError, SingleUpdateStatus]

  def getSubmissions(eoriNumber: EoriNumber): EitherT[Future, SubmissionServiceError, SubmissionSummaryList]

  def getSubmission(eoriNumber: EoriNumber, submissionId: SubmissionId): EitherT[Future, SubmissionServiceError, Submission]

  def cancelSubmission(eoriNumber: EoriNumber, submissionId: SubmissionId): EitherT[Future, SubmissionServiceError, SingleUpdateStatus]

  def updateNotification(
    notification: AesDigitalNotification
  ): EitherT[Future, SubmissionServiceError, SingleUpdateStatus]

@Singleton
class SubmissionServiceImpl @Inject() (
  aesIE507Repository: AesIE507Repository,
  aesIE507Factory:    AesIE507Factory,
  clock:              Clock
)(using ExecutionContext)
    extends SubmissionService:
  def getSubmissions(eoriNumber: EoriNumber): EitherT[Future, SubmissionServiceError, SubmissionSummaryList] =
    val submissionSummaryListResult: EitherT[Future, MongoError, SubmissionSummaryList] =
      aesIE507Repository
        .getMessages(eoriNumber)
        .map(messageSummariesNel =>
          SubmissionSummaryList(
            messageSummariesNel.toList.map(SubmissionSummary.fromMongoAesIE507MessageSummary)
          )
        )

    submissionSummaryListResult.leftFlatMap {
      case MongoError.DocumentNotFound(_) =>
        EitherT(Future.successful(Right(SubmissionSummaryList(Nil))))
      case me =>
        EitherT(
          Future.successful(
            Left(
              SubmissionService
                .MongoErrorMapper(s"EORI: ${eoriNumber.value}")
                .withRetrieveMongoError
                .apply(me)
            )
          )
        )
    }
  end getSubmissions

  def getSubmission(eoriNumber: EoriNumber, submissionId: SubmissionId): EitherT[Future, SubmissionServiceError, Submission] =
    val submissionResult: EitherT[Future, MongoError, Submission] =
      aesIE507Repository
        .getMessage(eoriNumber, submissionId)
        .map(Submission.fromMongoAesIE507Message)

    submissionResult.leftMap(
      SubmissionService
        .MongoErrorMapper(context = s"EORI: ${eoriNumber.value}, submissionId: ${submissionId.value}")
        .withRetrieveMongoError
        .apply
    )

  def cancelSubmission(eoriNumber: EoriNumber, submissionId: SubmissionId): EitherT[Future, SubmissionServiceError, SingleUpdateStatus] =
    aesIE507Repository
      .cancel(eoriNumber, submissionId, Instant.now(clock))
      .leftMap(
        SubmissionService
          .MongoErrorMapper(s"EORI: ${eoriNumber.value}, submissionId: ${submissionId.value}")
          .withUpdateMongoError
          .apply
      )

  def submitMessage(
    message:             AesIE507Message,
    exportOperationType: ExportOperationType,
    eoriNumber:          EoriNumber,
    maybeCorrelationId:  Option[HttpHeader.CorrelationId]
  ): EitherT[Future, SubmissionServiceError, SingleUpdateStatus] =
    val mongoMessage: MongoAesIE507Message =
      aesIE507Factory.mongoMessage(message, eoriNumber, exportOperationType, maybeCorrelationId)

    aesIE507Repository
      .submit(mongoMessage)
      .leftMap(me =>
        val context: String =
          Seq(
            Some(s"EORI: ${eoriNumber.value}"),
            message.submissionId.map(submissionId => s"submissionId: ${submissionId.value}")
          ).flatten.mkString(", ")

        SubmissionService
          .MongoErrorMapper(context)
          .withUpsertMongoError
          .apply(me)
      )

  def updateNotification(
    notification: AesDigitalNotification
  ): EitherT[Future, SubmissionServiceError, SingleUpdateStatus] =

    val eori: EoriNumber = EoriNumber(notification.eori)
    val mrn:  Mrn        = Mrn(notification.mrn)

    val context: String =
      s"EORI: ${notification.eori}, MRN: ${notification.mrn}, correlationId: ${notification.correlationId}"

    for
      submission <- aesIE507Repository
                      .getMessageByNotification(
                        eori,
                        mrn,
                        notification.correlationId
                      )
                      .leftMap(
                        SubmissionService
                          .MongoErrorMapper(context)
                          .withRetrieveMongoError
                          .apply
                      )

      status: NotificationEventStatus = notificationEventStatus(
                                          submission.exportOperation.exportOperationType,
                                          notification.status
                                        )

      result <- aesIE507Repository
                  .updateNotification(
                    eori,
                    mrn,
                    notification.correlationId,
                    Instant.now(clock),
                    status,
                    notification.notificationErrors
                  )
                  .leftMap(
                    SubmissionService
                      .MongoErrorMapper(context)
                      .withUpdateMongoError
                      .apply
                  )
    yield result

  private def notificationEventStatus(
    exportOperationType: ExportOperationType,
    notificationStatus:  NotificationStatus
  ): NotificationEventStatus =
    (exportOperationType, notificationStatus) match
      case (ExportOperationType.Standard, NotificationStatus.Accepted) =>
        NotificationEventStatus.Accepted

      case (ExportOperationType.Amend, NotificationStatus.Accepted) =>
        NotificationEventStatus.Amended

      case (ExportOperationType.Cancel, NotificationStatus.Accepted) =>
        NotificationEventStatus.Cancelled

      case (_, NotificationStatus.Rejected) =>
        NotificationEventStatus.Rejected

      case (_, NotificationStatus.Diversion) =>
        NotificationEventStatus.Awaiting
end SubmissionServiceImpl

object SubmissionService:
  final class MongoErrorMapper private (val context: String, private val mappers: PartialFunction[MongoError, SubmissionServiceError])
      extends AesErrorMapper(mappers):
    private def notFoundMongoErrorMapper: PartialFunction[MongoError, SubmissionServiceError] = { case _: MongoError.DocumentNotFound =>
      SubmissionServiceError.SubmissionNotFound(s"Submission not found. $context")
    }

    def withRetrieveMongoError: MongoErrorMapper =
      new MongoErrorMapper(
        context,
        withMapperAfter { case _ =>
          SubmissionServiceError.SubmissionOperationFailure(s"Submission retrieval failed. $context")
        }
      )

    def withUpdateMongoError: MongoErrorMapper =
      new MongoErrorMapper(
        context,
        withMapperAfter { case _ =>
          SubmissionServiceError.SubmissionOperationFailure(s"Submission update failed. $context")
        }
      )

    def withUpsertMongoError: MongoErrorMapper =
      new MongoErrorMapper(
        context,
        withMapperAfter { case _ =>
          SubmissionServiceError.SubmissionOperationFailure(s"Submission update/insert failed. $context")
        }
      )

    override def apply(mongoError: MongoError): SubmissionServiceError =
      withMapperBefore(notFoundMongoErrorMapper)
        .applyOrElse(
          mongoError,
          _ => SubmissionServiceError.SubmissionOperationFailure(s"Submission operation failed. $context")
        )
  end MongoErrorMapper

  object MongoErrorMapper:
    def apply(context: String): MongoErrorMapper =
      new MongoErrorMapper(context, PartialFunction.empty)
end SubmissionService
