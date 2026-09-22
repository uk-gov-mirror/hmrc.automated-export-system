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

import cats.data.{EitherT, NonEmptyList}
import helpers.EitherTFutureOps.{toEitherTLeft, toEitherTRight}
import org.mockito.Mockito.when
import org.scalatest.EitherValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar
import uk.gov.hmrc.automatedexportsystem.errors.{MongoError, SubmissionServiceError}
import uk.gov.hmrc.automatedexportsystem.models.IE507.*
import uk.gov.hmrc.automatedexportsystem.models.IE507.aes.{AesIE507Message, SubmissionId}
import uk.gov.hmrc.automatedexportsystem.models.http.HttpHeader
import uk.gov.hmrc.automatedexportsystem.models.mongo.SingleUpdateStatus
import uk.gov.hmrc.automatedexportsystem.models.mongo.read.MongoAesIE507MessageSummary
import uk.gov.hmrc.automatedexportsystem.models.mongo.write.MongoAesIE507Message
import uk.gov.hmrc.automatedexportsystem.models.notification.*
import uk.gov.hmrc.automatedexportsystem.models.notification.NotificationEventStatus.Awaiting
import uk.gov.hmrc.automatedexportsystem.models.responses.{Submission, SubmissionSummary, SubmissionSummaryList}
import uk.gov.hmrc.automatedexportsystem.repositories.AesIE507Repository
import uk.gov.hmrc.automatedexportsystem.util.IdGenerator

import java.time.*
import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

class SubmissionServiceSpec extends AnyFreeSpecLike, Matchers, EitherValues, ScalaFutures, MockitoSugar:
  given ec: ExecutionContext = ExecutionContext.global

  val aesIE507Repository: AesIE507Repository = mock[AesIE507Repository]

  val aesIE507Factory: AesIE507Factory = mock[AesIE507Factory]

  val instant: Instant = Instant.parse("2026-08-12T00:00:00.000Z")

  val clock: Clock = Clock.fixed(instant, ZoneOffset.UTC)

  val idGenerator: IdGenerator = mock[IdGenerator]

  val submissionService: SubmissionService = SubmissionServiceImpl(aesIE507Repository, aesIE507Factory, clock)

  object TestData:
    val eoriNumber:   EoriNumber   = EoriNumber("eoriNumber")
    val submissionId: SubmissionId =
      SubmissionId(UUID.fromString("6fb33641-6dc7-4a4f-adef-06238c13a317"))
    val correlationId: String        = "correlationId"
    val instant:       Instant       = Instant.parse("2026-08-12T00:00:00.000Z")
    val dateTime:      LocalDateTime = LocalDateTime.parse("2026-08-12T00:00:00")

    val mrn: Mrn = Mrn("mrn")

    val notification: AesDigitalNotification =
      AesDigitalNotification(
        correlationId = correlationId,
        eori = eoriNumber.value,
        mrn = mrn.value,
        dateCreated = dateTime,
        status = NotificationStatus.Accepted,
        notificationErrors = None
      )

    val correlationIdHeader: HttpHeader.CorrelationId = HttpHeader.CorrelationId(correlationId)

    val aesIE507Message: AesIE507Message =
      AesIE507Message(
        submissionId = Some(submissionId),
        exportOperation = ExportOperation(
          exportOperationType = ExportOperationType.Standard,
          mrn = Mrn("mrn"),
          discrepanciesExist = DiscrepanciesExist(true),
          splitIndicator = SplitIndicator(true)
        ),
        customsOfficeOfExitActual = CustomsOfficeOfExitActual(
          referenceNumber = ReferenceNumber("referenceNumber")
        ),
        goodsShipment = None
      )

    val notificationEvent1: NotificationEvent =
      NotificationEvent(
        correlationId = correlationId,
        dateCreated = instant,
        dateUpdated = instant,
        isPending = true,
        status = NotificationEventStatus.Awaiting,
        errors = None
      )

    val notificationEvent2: NotificationEvent =
      notificationEvent1.copy(
        dateUpdated = instant.plusMillis(1),
        isPending = false,
        status = NotificationEventStatus.Accepted
      )

    val notificationEvent3: NotificationEvent =
      notificationEvent1.copy(
        dateCreated = instant.plusMillis(1),
        dateUpdated = instant.plusMillis(1),
        isPending = false,
        status = NotificationEventStatus.Rejected,
        errors = Some(
          NonEmptyList.one(
            NotificationError(
              code = "code",
              description = "description",
              path = Some("path"),
              originalValue = Some("originalValue")
            )
          )
        )
      )

    val mongoAesIE507Message: MongoAesIE507Message =
      MongoAesIE507Message(
        submissionId = submissionId,
        eoriNumber = eoriNumber,
        createdAt = instant,
        updatedAt = instant,
        exportOperation = ExportOperation(
          exportOperationType = ExportOperationType.Standard,
          mrn = Mrn("mrn"),
          discrepanciesExist = DiscrepanciesExist(true),
          splitIndicator = SplitIndicator(true)
        ),
        customsOfficeOfExitActual = CustomsOfficeOfExitActual(
          referenceNumber = ReferenceNumber("referenceNumber")
        ),
        goodsShipment = None,
        metadata = NonEmptyList.of(
          notificationEvent1,
          notificationEvent2,
          notificationEvent3
        )
      )

    val mongoAesIE507MessageSummary: MongoAesIE507MessageSummary =
      MongoAesIE507MessageSummary(
        submissionId = submissionId,
        exportOperation = ExportOperation(
          exportOperationType = ExportOperationType.Standard,
          mrn = Mrn("mrn"),
          discrepanciesExist = DiscrepanciesExist(true),
          splitIndicator = SplitIndicator(true)
        ),
        customsOfficeOfExitActual = CustomsOfficeOfExitActual(
          referenceNumber = ReferenceNumber("referenceNumber")
        ),
        ducr = None,
        status = NotificationEventStatus.Awaiting,
        updatedAt = instant
      )
  end TestData

  "SubmissionService" - {

    ".getSubmissions" - {

      "should return a list of submissions" - {

        "when no submission with the given EORI can be found in the mongodb collection" in {
          implicitly[Ordering[Instant]]
          when(aesIE507Repository.getMessages(TestData.eoriNumber))
            .thenReturn(EitherT(Future.successful(Left(MongoError.DocumentNotFound("")))))

          val result: SubmissionSummaryList =
            submissionService.getSubmissions(TestData.eoriNumber).value.futureValue.value

          result.submissions shouldBe Nil
        }

        "when there are multiple submissions with the given EORI in the mongodb collection" in {
          val mongoAesIE507MessageSummaries: Seq[MongoAesIE507MessageSummary] =
            Seq.fill(3)(TestData.mongoAesIE507MessageSummary)

          val submissionSummary: SubmissionSummary =
            SubmissionSummary(
              submissionId = TestData.submissionId,
              mrn = Mrn("mrn"),
              ducr = None,
              officeOfExitCode = ReferenceNumber("referenceNumber"),
              updatedAt = TestData.dateTime,
              status = Awaiting
            )

          val submissionSummaryList: List[SubmissionSummary] =
            List.fill(3)(submissionSummary)

          when(aesIE507Repository.getMessages(TestData.eoriNumber))
            .thenReturn(
              EitherT(
                Future.successful(
                  Right(
                    NonEmptyList.of(
                      mongoAesIE507MessageSummaries.head,
                      mongoAesIE507MessageSummaries.tail*
                    )
                  )
                )
              )
            )

          val result: SubmissionSummaryList =
            submissionService.getSubmissions(TestData.eoriNumber).value.futureValue.value

          result.submissions shouldBe submissionSummaryList
        }
      }

      "should return an error" - {

        "when the retrieval operation returns an unexpected error" in {
          when(aesIE507Repository.getMessages(TestData.eoriNumber))
            .thenReturn(EitherT(Future.successful(Left(MongoError.UnexpectedError(Exception())))))

          val error: SubmissionServiceError =
            SubmissionServiceError.SubmissionOperationFailure(
              s"Submission retrieval failed. EORI: ${TestData.eoriNumber.value}"
            )

          val result: SubmissionServiceError =
            submissionService.getSubmissions(TestData.eoriNumber).value.futureValue.left.value

          result shouldBe error
        }
      }
    }

    ".getSubmission" - {

      "should return a single submission" - {

        "when one submission with the given EORI and submissionId is found in the mongodb collection" - {

          "and there are multiple NotificationEvent in the submission, will select the most recent" in {
            when(aesIE507Repository.getMessage(TestData.eoriNumber, TestData.submissionId))
              .thenReturn(EitherT(Future.successful(Right(TestData.mongoAesIE507Message))))

            val submission: Submission =
              Submission(
                submissionId = TestData.submissionId,
                status = NotificationEventStatus.Rejected,
                exportOperation = TestData.mongoAesIE507Message.exportOperation,
                customsOfficeOfExitActual = TestData.mongoAesIE507Message.customsOfficeOfExitActual,
                goodsShipment = TestData.mongoAesIE507Message.goodsShipment,
                updatedAt = TestData.dateTime,
                metadata = Some(
                  NonEmptyList.one(
                    NotificationError(
                      code = "code",
                      description = "description",
                      path = Some("path"),
                      originalValue = Some("originalValue")
                    )
                  )
                )
              )

            val result: Submission =
              submissionService.getSubmission(TestData.eoriNumber, TestData.submissionId).value.futureValue.value

            result shouldBe submission
          }
        }
      }

      "should return an error" - {

        "when there are no submissions with the given EORI and submissionId found in the mongodb collection" in {
          when(aesIE507Repository.getMessage(TestData.eoriNumber, TestData.submissionId))
            .thenReturn(EitherT(Future.successful(Left(MongoError.DocumentNotFound("")))))

          val error: SubmissionServiceError =
            SubmissionServiceError.SubmissionNotFound(
              s"Submission not found. EORI: ${TestData.eoriNumber.value}, " +
                s"submissionId: ${TestData.submissionId.value.toString}"
            )

          val result: SubmissionServiceError =
            submissionService.getSubmission(TestData.eoriNumber, TestData.submissionId).value.futureValue.left.value

          result shouldBe error
        }

        "when the retrieval operation returns an unexpected error" in {
          when(aesIE507Repository.getMessage(TestData.eoriNumber, TestData.submissionId))
            .thenReturn(EitherT(Future.successful(Left(MongoError.UnexpectedError(Exception())))))

          val error: SubmissionServiceError =
            SubmissionServiceError.SubmissionOperationFailure(
              s"Submission retrieval failed. EORI: ${TestData.eoriNumber.value}, " +
                s"submissionId: ${TestData.submissionId.value.toString}"
            )

          val result: SubmissionServiceError =
            submissionService.getSubmission(TestData.eoriNumber, TestData.submissionId).value.futureValue.left.value

          result shouldBe error
        }
      }
    }

    ".submitMessage" - {

      "should insert a submission" - {

        "when there is no submission with the given submissionId found in the mongodb collection" in {
          when(
            aesIE507Factory.mongoMessage(
              TestData.aesIE507Message,
              TestData.eoriNumber,
              ExportOperationType.Standard,
              Some(TestData.correlationIdHeader)
            )
          )
            .thenReturn(TestData.mongoAesIE507Message)

          when(aesIE507Repository.submit(TestData.mongoAesIE507Message))
            .thenReturn(SingleUpdateStatus.Upserted("submitUpsert").toEitherTRight[MongoError])

          val result: SingleUpdateStatus =
            submissionService
              .submitMessage(
                TestData.aesIE507Message,
                ExportOperationType.Standard,
                TestData.eoriNumber,
                Some(TestData.correlationIdHeader)
              )
              .value
              .futureValue
              .value

          result shouldBe SingleUpdateStatus.Upserted("submitUpsert")
        }
      }

      "should replace a submission" - {

        "when a submission with the given submissionId is found in the mongodb collection" in {
          when(
            aesIE507Factory.mongoMessage(
              TestData.aesIE507Message,
              TestData.eoriNumber,
              ExportOperationType.Standard,
              Some(TestData.correlationIdHeader)
            )
          )
            .thenReturn(TestData.mongoAesIE507Message)

          when(aesIE507Repository.submit(TestData.mongoAesIE507Message))
            .thenReturn(SingleUpdateStatus.Updated("submitUpsert").toEitherTRight[MongoError])

          val result: SingleUpdateStatus =
            submissionService
              .submitMessage(
                TestData.aesIE507Message,
                ExportOperationType.Standard,
                TestData.eoriNumber,
                Some(TestData.correlationIdHeader)
              )
              .value
              .futureValue
              .value

          result shouldBe SingleUpdateStatus.Updated("submitUpsert")
        }
      }

      "should return an error" - {

        "when the upsert operation returns an unexpected error" in {
          when(
            aesIE507Factory.mongoMessage(
              TestData.aesIE507Message,
              TestData.eoriNumber,
              ExportOperationType.Standard,
              Some(TestData.correlationIdHeader)
            )
          )
            .thenReturn(TestData.mongoAesIE507Message)

          when(aesIE507Repository.submit(TestData.mongoAesIE507Message))
            .thenReturn(MongoError.UnexpectedError(Exception()).toEitherTLeft[SingleUpdateStatus])

          val error: SubmissionServiceError =
            SubmissionServiceError.SubmissionOperationFailure(
              s"Submission update/insert failed. EORI: ${TestData.eoriNumber.value}, " +
                s"submissionId: ${TestData.submissionId.value.toString}"
            )

          val result: SubmissionServiceError =
            submissionService
              .submitMessage(
                TestData.aesIE507Message,
                ExportOperationType.Standard,
                TestData.eoriNumber,
                Some(TestData.correlationIdHeader)
              )
              .value
              .futureValue
              .left
              .value

          result shouldBe error
        }
      }
    }

    ".cancelSubmission" - {

      "should cancel a submission" - {

        "when a submission with the given EORI and submissionId is found in the mongodb collection" - {

          "and the submission is not cancelled yet" in {
            when(aesIE507Repository.cancel(TestData.eoriNumber, TestData.submissionId, instant))
              .thenReturn(EitherT(Future.successful(Right(SingleUpdateStatus.Updated("cancel")))))

            val result: SingleUpdateStatus =
              submissionService.cancelSubmission(TestData.eoriNumber, TestData.submissionId).value.futureValue.value

            result shouldBe SingleUpdateStatus.Updated("cancel")
          }

          "and the submission is already cancelled" in {
            when(aesIE507Repository.cancel(TestData.eoriNumber, TestData.submissionId, instant))
              .thenReturn(EitherT(Future.successful(Right(SingleUpdateStatus.AlreadyUpToDate("cancel")))))

            val result: SingleUpdateStatus =
              submissionService.cancelSubmission(TestData.eoriNumber, TestData.submissionId).value.futureValue.value

            result shouldBe SingleUpdateStatus.AlreadyUpToDate("cancel")
          }
        }
      }

      "should return an error" - {

        "when there is no submission with the given submissionId found in the mongodb collection" in {
          when(aesIE507Repository.cancel(TestData.eoriNumber, TestData.submissionId, instant))
            .thenReturn(EitherT(Future.successful(Left(MongoError.DocumentNotFound("")))))

          val error: SubmissionServiceError =
            SubmissionServiceError.SubmissionNotFound(
              s"Submission not found. EORI: ${TestData.eoriNumber.value}, " +
                s"submissionId: ${TestData.submissionId.value}"
            )

          val result: SubmissionServiceError =
            submissionService.cancelSubmission(TestData.eoriNumber, TestData.submissionId).value.futureValue.left.value

          result shouldBe error
        }

        "when the update operation returns an unexpected error" in {
          when(aesIE507Repository.cancel(TestData.eoriNumber, TestData.submissionId, instant))
            .thenReturn(EitherT(Future.successful(Left(MongoError.UnexpectedError(Exception())))))

          val error: SubmissionServiceError =
            SubmissionServiceError.SubmissionOperationFailure(
              s"Submission update failed. EORI: ${TestData.eoriNumber.value}, " +
                s"submissionId: ${TestData.submissionId.value}"
            )

          val result: SubmissionServiceError =
            submissionService.cancelSubmission(TestData.eoriNumber, TestData.submissionId).value.futureValue.left.value

          result shouldBe error
        }
      }
    }

    ".updateNotification" - {

      "should update the notification event" - {

        "with Accepted status for a Standard submission" in {

          val notification =
            TestData.notification.copy(
              status = NotificationStatus.Accepted
            )

          val mongoMessage =
            TestData.mongoAesIE507Message.copy(
              exportOperation = TestData.mongoAesIE507Message.exportOperation.copy(
                exportOperationType = ExportOperationType.Standard
              )
            )

          when(
            aesIE507Repository.getMessageByNotification(
              TestData.eoriNumber,
              TestData.mrn,
              TestData.correlationId
            )
          )
            .thenReturn(mongoMessage.toEitherTRight[MongoError])

          when(
            aesIE507Repository.updateNotification(
              TestData.eoriNumber,
              TestData.mrn,
              TestData.correlationId,
              instant,
              NotificationEventStatus.Accepted,
              None
            )
          )
            .thenReturn(SingleUpdateStatus.Updated("updateNotification").toEitherTRight[MongoError])

          val result =
            submissionService
              .updateNotification(notification)
              .value
              .futureValue
              .value

          result shouldBe SingleUpdateStatus.Updated("updateNotification")
        }

        "with Amended status for an Amend submission" in {

          val notification =
            TestData.notification.copy(
              status = NotificationStatus.Accepted
            )

          val mongoMessage =
            TestData.mongoAesIE507Message.copy(
              exportOperation = TestData.mongoAesIE507Message.exportOperation.copy(
                exportOperationType = ExportOperationType.Amend
              )
            )

          when(
            aesIE507Repository.getMessageByNotification(
              TestData.eoriNumber,
              TestData.mrn,
              TestData.correlationId
            )
          )
            .thenReturn(mongoMessage.toEitherTRight[MongoError])

          when(
            aesIE507Repository.updateNotification(
              TestData.eoriNumber,
              TestData.mrn,
              TestData.correlationId,
              instant,
              NotificationEventStatus.Amended,
              None
            )
          )
            .thenReturn(SingleUpdateStatus.Updated("updateNotification").toEitherTRight[MongoError])

          val result =
            submissionService
              .updateNotification(notification)
              .value
              .futureValue
              .value

          result shouldBe SingleUpdateStatus.Updated("updateNotification")
        }

        "with Cancelled status for a Cancel submission" in {

          val notification =
            TestData.notification.copy(
              status = NotificationStatus.Accepted
            )

          val mongoMessage =
            TestData.mongoAesIE507Message.copy(
              exportOperation = TestData.mongoAesIE507Message.exportOperation.copy(
                exportOperationType = ExportOperationType.Cancel
              )
            )

          when(
            aesIE507Repository.getMessageByNotification(
              TestData.eoriNumber,
              TestData.mrn,
              TestData.correlationId
            )
          )
            .thenReturn(mongoMessage.toEitherTRight[MongoError])

          when(
            aesIE507Repository.updateNotification(
              TestData.eoriNumber,
              TestData.mrn,
              TestData.correlationId,
              instant,
              NotificationEventStatus.Cancelled,
              None
            )
          )
            .thenReturn(SingleUpdateStatus.Updated("updateNotification").toEitherTRight[MongoError])

          val result =
            submissionService
              .updateNotification(notification)
              .value
              .futureValue
              .value

          result shouldBe SingleUpdateStatus.Updated("updateNotification")
        }

        "with Rejected status when the notification is rejected" in {

          val notification =
            TestData.notification.copy(
              status = NotificationStatus.Rejected
            )

          when(
            aesIE507Repository.getMessageByNotification(
              TestData.eoriNumber,
              TestData.mrn,
              TestData.correlationId
            )
          )
            .thenReturn(TestData.mongoAesIE507Message.toEitherTRight[MongoError])

          when(
            aesIE507Repository.updateNotification(
              TestData.eoriNumber,
              TestData.mrn,
              TestData.correlationId,
              instant,
              NotificationEventStatus.Rejected,
              None
            )
          )
            .thenReturn(SingleUpdateStatus.Updated("updateNotification").toEitherTRight[MongoError])

          val result =
            submissionService
              .updateNotification(notification)
              .value
              .futureValue
              .value

          result shouldBe SingleUpdateStatus.Updated("updateNotification")
        }

        "with Awaiting status when the notification is a diversion" in {

          val notification =
            TestData.notification.copy(
              status = NotificationStatus.Diversion
            )

          when(
            aesIE507Repository.getMessageByNotification(
              TestData.eoriNumber,
              TestData.mrn,
              TestData.correlationId
            )
          )
            .thenReturn(TestData.mongoAesIE507Message.toEitherTRight[MongoError])

          when(
            aesIE507Repository.updateNotification(
              TestData.eoriNumber,
              TestData.mrn,
              TestData.correlationId,
              instant,
              NotificationEventStatus.Awaiting,
              None
            )
          )
            .thenReturn(SingleUpdateStatus.Updated("updateNotification").toEitherTRight[MongoError])

          val result =
            submissionService
              .updateNotification(notification)
              .value
              .futureValue
              .value

          result shouldBe SingleUpdateStatus.Updated("updateNotification")
        }
      }

      "should return an error" - {

        "when retrieving the submission fails" in {

          val mongoError =
            MongoError.UnexpectedError(Exception("Unexpected error"))

          when(
            aesIE507Repository.getMessageByNotification(
              TestData.eoriNumber,
              TestData.mrn,
              TestData.correlationId
            )
          )
            .thenReturn(mongoError.toEitherTLeft[MongoAesIE507Message])

          val result: SubmissionServiceError =
            submissionService
              .updateNotification(TestData.notification)
              .value
              .futureValue
              .left
              .value

          result shouldBe SubmissionServiceError.SubmissionOperationFailure(
            s"Submission retrieval failed. EORI: ${TestData.eoriNumber.value}, " +
              s"MRN: ${TestData.mrn.value}, correlationId: ${TestData.correlationId}"
          )
        }

        "when updating the notification fails" in {

          when(
            aesIE507Repository.getMessageByNotification(
              TestData.eoriNumber,
              TestData.mrn,
              TestData.correlationId
            )
          )
            .thenReturn(TestData.mongoAesIE507Message.toEitherTRight[MongoError])

          val mongoError =
            MongoError.UnexpectedError(Exception("Unexpected error"))

          when(
            aesIE507Repository.updateNotification(
              TestData.eoriNumber,
              TestData.mrn,
              TestData.correlationId,
              instant,
              NotificationEventStatus.Accepted,
              None
            )
          )
            .thenReturn(mongoError.toEitherTLeft[SingleUpdateStatus])

          val result: SubmissionServiceError =
            submissionService
              .updateNotification(TestData.notification)
              .value
              .futureValue
              .left
              .value

          result shouldBe SubmissionServiceError.SubmissionOperationFailure(
            s"Submission update failed. EORI: ${TestData.eoriNumber.value}, " +
              s"MRN: ${TestData.mrn.value}, correlationId: ${TestData.correlationId}"
          )
        }

        "when the submission cannot be found" in {

          val mongoError =
            MongoError.DocumentNotFound("Document not found")

          when(
            aesIE507Repository.getMessageByNotification(
              TestData.eoriNumber,
              TestData.mrn,
              TestData.correlationId
            )
          )
            .thenReturn(mongoError.toEitherTLeft[MongoAesIE507Message])

          val result: SubmissionServiceError =
            submissionService
              .updateNotification(TestData.notification)
              .value
              .futureValue
              .left
              .value

          result shouldBe SubmissionServiceError.SubmissionNotFound(
            s"Submission not found. EORI: ${TestData.eoriNumber.value}, " +
              s"MRN: ${TestData.mrn.value}, correlationId: ${TestData.correlationId}"
          )
        }
      }
    }
  }
