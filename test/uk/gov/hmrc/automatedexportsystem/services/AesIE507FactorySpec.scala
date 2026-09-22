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
import org.mockito.Mockito.when
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar
import uk.gov.hmrc.automatedexportsystem.models.IE507.*
import uk.gov.hmrc.automatedexportsystem.models.IE507.aes.{AesIE507Message, SubmissionId}
import uk.gov.hmrc.automatedexportsystem.models.http.HttpHeader
import uk.gov.hmrc.automatedexportsystem.models.mongo.write.MongoAesIE507Message
import uk.gov.hmrc.automatedexportsystem.models.notification.{NotificationEvent, NotificationEventStatus}
import uk.gov.hmrc.automatedexportsystem.util.IdGenerator

import java.time.{Clock, Instant, ZoneOffset}
import java.util.UUID

class AesIE507FactorySpec extends AnyFreeSpecLike, Matchers, MockitoSugar:
  object TestData:
    val uuid:          UUID       = UUID.fromString("6fb33641-6dc7-4a4f-adef-06238c13a317")
    val instant:       Instant    = Instant.parse("2026-08-24T00:00:00.000Z")
    val eoriNumber:    EoriNumber = EoriNumber("eoriNumber")
    val correlationId: String     = "correlationId"

    val correlationIdHeader: HttpHeader.CorrelationId = HttpHeader.CorrelationId(correlationId)

    val aesIE507Message: AesIE507Message =
      AesIE507Message(
        submissionId = Some(SubmissionId(uuid)),
        exportOperation = ExportOperation(
          exportOperationType = ExportOperationType.Standard,
          mrn = Mrn("mrn"),
          discrepanciesExist = DiscrepanciesExist(false),
          splitIndicator = SplitIndicator(true)
        ),
        customsOfficeOfExitActual = CustomsOfficeOfExitActual(
          referenceNumber = ReferenceNumber("referenceNumber")
        ),
        goodsShipment = None
      )
  end TestData

  trait Setup:
    val clock: Clock = Clock.fixed(TestData.instant, ZoneOffset.UTC)

    val idGenerator: IdGenerator = mock[IdGenerator]

    val aesIE507Factory: AesIE507Factory = AesIE507Factory(clock, idGenerator)
  end Setup

  "AesIE507Factory" - {

    ".mongoMessage" - {

      "should return a MongoAesIE507Message" - {

        "when submissionId is provided" in new Setup {
          val mongoAesIE507Message: MongoAesIE507Message =
            MongoAesIE507Message(
              submissionId = SubmissionId(TestData.uuid),
              eoriNumber = TestData.eoriNumber,
              createdAt = TestData.instant,
              updatedAt = TestData.instant,
              exportOperation = ExportOperation(
                exportOperationType = ExportOperationType.Standard,
                mrn = Mrn("mrn"),
                discrepanciesExist = DiscrepanciesExist(false),
                splitIndicator = SplitIndicator(true)
              ),
              customsOfficeOfExitActual = CustomsOfficeOfExitActual(
                referenceNumber = ReferenceNumber("referenceNumber")
              ),
              goodsShipment = None,
              metadata = NonEmptyList.one(
                NotificationEvent(
                  correlationId = TestData.correlationId,
                  dateCreated = TestData.instant,
                  dateUpdated = TestData.instant,
                  isPending = true,
                  status = NotificationEventStatus.Awaiting,
                  errors = None
                )
              )
            )

          val result: MongoAesIE507Message =
            aesIE507Factory.mongoMessage(
              TestData.aesIE507Message,
              TestData.eoriNumber,
              ExportOperationType.Standard,
              Some(TestData.correlationIdHeader)
            )

          result shouldBe mongoAesIE507Message
        }

        "when submissionId is missing" in new Setup {
          when(idGenerator.generate)
            .thenReturn(TestData.uuid)

          val mongoAesIE507Message: MongoAesIE507Message =
            MongoAesIE507Message(
              submissionId = SubmissionId(TestData.uuid),
              eoriNumber = TestData.eoriNumber,
              createdAt = TestData.instant,
              updatedAt = TestData.instant,
              exportOperation = ExportOperation(
                exportOperationType = ExportOperationType.Standard,
                mrn = Mrn("mrn"),
                discrepanciesExist = DiscrepanciesExist(false),
                splitIndicator = SplitIndicator(true)
              ),
              customsOfficeOfExitActual = CustomsOfficeOfExitActual(
                referenceNumber = ReferenceNumber("referenceNumber")
              ),
              goodsShipment = None,
              metadata = NonEmptyList.one(
                NotificationEvent(
                  correlationId = TestData.correlationId,
                  dateCreated = TestData.instant,
                  dateUpdated = TestData.instant,
                  isPending = true,
                  status = NotificationEventStatus.Awaiting,
                  errors = None
                )
              )
            )

          val result: MongoAesIE507Message =
            aesIE507Factory.mongoMessage(
              TestData.aesIE507Message.copy(submissionId = None),
              TestData.eoriNumber,
              ExportOperationType.Standard,
              Some(TestData.correlationIdHeader)
            )

          result shouldBe mongoAesIE507Message
        }

        "when correlationId is provided" in new Setup {
          val mongoAesIE507Message: MongoAesIE507Message =
            MongoAesIE507Message(
              submissionId = SubmissionId(TestData.uuid),
              eoriNumber = TestData.eoriNumber,
              createdAt = TestData.instant,
              updatedAt = TestData.instant,
              exportOperation = ExportOperation(
                exportOperationType = ExportOperationType.Standard,
                mrn = Mrn("mrn"),
                discrepanciesExist = DiscrepanciesExist(false),
                splitIndicator = SplitIndicator(true)
              ),
              customsOfficeOfExitActual = CustomsOfficeOfExitActual(
                referenceNumber = ReferenceNumber("referenceNumber")
              ),
              goodsShipment = None,
              metadata = NonEmptyList.one(
                NotificationEvent(
                  correlationId = TestData.correlationId,
                  dateCreated = TestData.instant,
                  dateUpdated = TestData.instant,
                  isPending = true,
                  status = NotificationEventStatus.Awaiting,
                  errors = None
                )
              )
            )

          val result: MongoAesIE507Message =
            aesIE507Factory.mongoMessage(
              TestData.aesIE507Message,
              TestData.eoriNumber,
              ExportOperationType.Standard,
              Some(TestData.correlationIdHeader)
            )

          result shouldBe mongoAesIE507Message
        }

        "when correlationId is missing" in new Setup {
          when(idGenerator.generate35Char)
            .thenReturn(TestData.correlationId)

          val mongoAesIE507Message: MongoAesIE507Message =
            MongoAesIE507Message(
              submissionId = SubmissionId(TestData.uuid),
              eoriNumber = TestData.eoriNumber,
              createdAt = TestData.instant,
              updatedAt = TestData.instant,
              exportOperation = ExportOperation(
                exportOperationType = ExportOperationType.Standard,
                mrn = Mrn("mrn"),
                discrepanciesExist = DiscrepanciesExist(false),
                splitIndicator = SplitIndicator(true)
              ),
              customsOfficeOfExitActual = CustomsOfficeOfExitActual(
                referenceNumber = ReferenceNumber("referenceNumber")
              ),
              goodsShipment = None,
              metadata = NonEmptyList.one(
                NotificationEvent(
                  correlationId = TestData.correlationId,
                  dateCreated = TestData.instant,
                  dateUpdated = TestData.instant,
                  isPending = true,
                  status = NotificationEventStatus.Awaiting,
                  errors = None
                )
              )
            )

          val result: MongoAesIE507Message =
            aesIE507Factory.mongoMessage(
              TestData.aesIE507Message,
              TestData.eoriNumber,
              ExportOperationType.Standard,
              None
            )

          result shouldBe mongoAesIE507Message
        }
      }
    }
  }
