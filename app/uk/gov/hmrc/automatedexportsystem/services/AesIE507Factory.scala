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

import cats.data.NonEmptyList
import play.api.Logging
import uk.gov.hmrc.automatedexportsystem.models.IE507.aes.{AesIE507Message, SubmissionId}
import uk.gov.hmrc.automatedexportsystem.models.IE507.{EoriNumber, ExportOperationType}
import uk.gov.hmrc.automatedexportsystem.models.http.HttpHeader
import uk.gov.hmrc.automatedexportsystem.models.mongo.write.MongoAesIE507Message
import uk.gov.hmrc.automatedexportsystem.models.notification.{NotificationEvent, NotificationEventStatus}
import uk.gov.hmrc.automatedexportsystem.util.IdGenerator

import java.time.{Clock, Instant}
import javax.inject.{Inject, Singleton}

@Singleton
class AesIE507Factory @Inject() (clock: Clock, idGenerator: IdGenerator) extends Logging:
  def mongoMessage(
    aesIE507Message:     AesIE507Message,
    eoriNumber:          EoriNumber,
    exportOperationType: ExportOperationType,
    maybeCorrelationId:  Option[HttpHeader.CorrelationId]
  ): MongoAesIE507Message =
    logger.info(
      s"Converting AesIE507Message to MongoAesIE507Message with " +
        s"submissionId: ${aesIE507Message.submissionId.getOrElse("None")}, " +
        s"eoriNumber: $eoriNumber"
    )

    val instantNow: Instant = Instant.now(clock)

    val correlationId: HttpHeader.CorrelationId =
      maybeCorrelationId.getOrElse(HttpHeader.CorrelationId(idGenerator.generate35Char))

    val submissionId: SubmissionId =
      aesIE507Message.submissionId.getOrElse(SubmissionId(idGenerator.generate))

    val notificationEvent: NotificationEvent =
      NotificationEvent(
        correlationId = correlationId.value,
        dateCreated = instantNow,
        dateUpdated = instantNow,
        isPending = true,
        status = NotificationEventStatus.Awaiting,
        errors = None
      )

    MongoAesIE507Message(
      submissionId = submissionId,
      eoriNumber = eoriNumber,
      createdAt = instantNow,
      updatedAt = instantNow,
      exportOperation = aesIE507Message.exportOperation.copy(
        exportOperationType = exportOperationType
      ),
      customsOfficeOfExitActual = aesIE507Message.customsOfficeOfExitActual,
      goodsShipment = aesIE507Message.goodsShipment,
      metadata = NonEmptyList.one(notificationEvent)
    )
  end mongoMessage
end AesIE507Factory
