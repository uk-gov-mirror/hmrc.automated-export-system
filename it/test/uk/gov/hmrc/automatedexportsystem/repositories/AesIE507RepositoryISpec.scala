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

package uk.gov.hmrc.automatedexportsystem.repositories

import cats.data.NonEmptyList
import org.mockito.Mockito.when
import org.mongodb.scala.model.{Filters, Indexes}
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.{EitherValues, OptionValues}
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import uk.gov.hmrc.automatedexportsystem.config.AppConfig
import uk.gov.hmrc.automatedexportsystem.errors.MongoError
import uk.gov.hmrc.automatedexportsystem.generators.MongoAesIE507MessageGenerator
import uk.gov.hmrc.automatedexportsystem.models.IE507.aes.SubmissionId
import uk.gov.hmrc.automatedexportsystem.models.IE507.{EoriNumber, ExportOperationType, Mrn}
import uk.gov.hmrc.automatedexportsystem.models.mongo.SingleUpdateStatus
import uk.gov.hmrc.automatedexportsystem.models.mongo.read.MongoAesIE507MessageSummary
import uk.gov.hmrc.automatedexportsystem.models.mongo.write.MongoAesIE507Message
import uk.gov.hmrc.automatedexportsystem.models.notification.{NotificationError, NotificationEvent, NotificationEventStatus}
import uk.gov.hmrc.mongo.test.DefaultPlayMongoRepositorySupport

import java.time.Instant
import java.time.temporal.ChronoUnit
import java.util.UUID
import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext

class AesIE507RepositoryISpec
    extends AnyFreeSpecLike,
      Matchers,
      EitherValues,
      OptionValues,
      MockitoSugar,
      ScalaCheckDrivenPropertyChecks,
      MongoAesIE507MessageGenerator,
      DefaultPlayMongoRepositorySupport[MongoAesIE507Message]:
  given ec: ExecutionContext = ExecutionContext.global

  val appConfig: AppConfig = mock[AppConfig]

  when(appConfig.replaceIndexes).thenReturn(true)
  when(appConfig.documentTtl).thenReturn(1L)

  protected val repository: AesIE507RepositoryImpl = AesIE507RepositoryImpl(mongoComponent, appConfig)

  object TestData:
    val instant: Instant = Instant.parse("2026-08-17T00:00:00.000Z")

    val submissionId: SubmissionId = SubmissionId(UUID.fromString("6fb33641-6dc7-4a4f-adef-06238c13a317"))

    val eoriNumber: EoriNumber = EoriNumber("eoriNumber")

    val mrn: Mrn = Mrn("mrn")

    val correlationId: String = "correlationId"

    def mongoAesIE507MessageSummary(message: MongoAesIE507Message) =
      MongoAesIE507MessageSummary(
        submissionId = message.submissionId,
        exportOperation = message.exportOperation,
        customsOfficeOfExitActual = message.customsOfficeOfExitActual,
        ducr = message.goodsShipment.map(_.consignment.referenceNumberUCR),
        updatedAt = message.updatedAt,
        status = message.metadata.toList
          .maxBy(event => (event.dateUpdated, event.dateCreated))
          .status
      )

  "AesIE507Repository" - {
    import helpers.GenHelpers.*

    "should have the expected TTL associated with the updatedAt index" in {
      val ttlIndex = repository.indexes.find(_.getKeys == Indexes.ascending("updatedAt")).head
      ttlIndex.getOptions.getExpireAfter(TimeUnit.SECONDS) shouldBe appConfig.documentTtl
    }

    "should be able to insert and retrieve documents" in
      forAll { (message: MongoAesIE507Message) =>
        insert(message).futureValue

        find(
          Filters.and(
            Filters.eq("eoriNumber", message.eoriNumber.value),
            Filters.eq("submissionId", message.submissionId.value.toString)
          )
        ).futureValue shouldBe Seq(message)
      }

    ".submit" - {

      "should replace a document with the given submissionId" - {

        "when there is a document in the collection with that submissionId" in {
          val mongoAesIE507Message: MongoAesIE507Message =
            arbitrary[MongoAesIE507Message]
              .withSubmissionId(TestData.submissionId)
              .sample
              .value

          val mongoAesIE507MessageReplacement: MongoAesIE507Message =
            mongoAesIE507Message.copy(updatedAt = mongoAesIE507Message.updatedAt.plusMillis(1))

          repository.collection.insertOne(mongoAesIE507Message).head().futureValue

          val updateStatus: SingleUpdateStatus =
            repository.submit(mongoAesIE507MessageReplacement).value.futureValue.value

          updateStatus shouldBe SingleUpdateStatus.Updated("submitUpsert")

          val result: Seq[MongoAesIE507Message] =
            find(Filters.eq("submissionId", TestData.submissionId.value.toString)).futureValue

          result shouldBe Seq(mongoAesIE507MessageReplacement)
        }
      }

      "should insert a document" - {

        "when there is no document in the collection with that submissionId" in {
          val mongoAesIE507Message: MongoAesIE507Message =
            arbitrary[MongoAesIE507Message]
              .withSubmissionId(TestData.submissionId)
              .sample
              .value

          val updateStatus: SingleUpdateStatus =
            repository.submit(mongoAesIE507Message).value.futureValue.value

          updateStatus shouldBe SingleUpdateStatus.Upserted("submitUpsert")

          val result: Seq[MongoAesIE507Message] =
            find(Filters.eq("submissionId", TestData.submissionId.value.toString)).futureValue

          result shouldBe Seq(mongoAesIE507Message)
        }
      }
    }

    ".getMessages" - {

      "should return all documents with the given eori from the collection" - {

        "when there is only one document in the collection with that eori" in {
          val mongoAesIE507MessagesDifferentEori: Seq[MongoAesIE507Message] =
            Seq.fill(2)(arbitrary[MongoAesIE507Message].sample).flatten

          val mongoAesIE507MessagesMatchingEori: Seq[MongoAesIE507Message] =
            Seq
              .fill(1)(
                arbitrary[MongoAesIE507Message]
                  .withEoriAndStatus(
                    TestData.eoriNumber,
                    NotificationEventStatus.Rejected
                  )
                  .sample
              )
              .flatten

          val mongoAesIE507Messages: Seq[MongoAesIE507Message] =
            mongoAesIE507MessagesDifferentEori ++ mongoAesIE507MessagesMatchingEori

          val mongoAesIE507MessageSummaries: Seq[MongoAesIE507MessageSummary] =
            mongoAesIE507MessagesMatchingEori.map(TestData.mongoAesIE507MessageSummary)

          repository.collection.insertMany(mongoAesIE507Messages).head().futureValue

          val result: NonEmptyList[MongoAesIE507MessageSummary] =
            repository.getMessages(TestData.eoriNumber).value.futureValue.value

          result.length shouldBe 1
          result.toList shouldBe mongoAesIE507MessageSummaries
        }

        "when there are multiple documents in the collection with that eori" in {
          val mongoAesIE507MessagesDifferentEori: Seq[MongoAesIE507Message] =
            Seq.fill(2)(arbitrary[MongoAesIE507Message].sample).flatten

          val mongoAesIE507MessagesMatchingEori: Seq[MongoAesIE507Message] =
            Seq
              .fill(2)(
                arbitrary[MongoAesIE507Message]
                  .withEoriAndStatus(
                    TestData.eoriNumber,
                    NotificationEventStatus.Accepted
                  )
                  .sample
              )
              .flatten

          val mongoAesIE507Messages: Seq[MongoAesIE507Message] =
            mongoAesIE507MessagesDifferentEori ++ mongoAesIE507MessagesMatchingEori

          val mongoAesIE507MessageSummaries: Seq[MongoAesIE507MessageSummary] =
            mongoAesIE507MessagesMatchingEori.map(TestData.mongoAesIE507MessageSummary)

          repository.collection.insertMany(mongoAesIE507Messages).head().futureValue

          val result: NonEmptyList[MongoAesIE507MessageSummary] =
            repository.getMessages(TestData.eoriNumber).value.futureValue.value

          result.length shouldBe 2
          result.toList   should contain theSameElementsAs mongoAesIE507MessageSummaries
        }
      }

      "should return a MongoError" - {

        "when there are no documents in the collection with that eori" in {
          val mongoAesIE507MessagesDifferentEori: Seq[MongoAesIE507Message] =
            Seq.fill(3)(arbitrary[MongoAesIE507Message].sample).flatten

          repository.collection.insertMany(mongoAesIE507MessagesDifferentEori).head().futureValue

          val result: MongoError =
            repository.getMessages(TestData.eoriNumber).value.futureValue.left.value

          val error: MongoError = MongoError.DocumentNotFound(s"No documents found for EORI: ${TestData.eoriNumber.value}")

          result shouldBe error
        }
      }
    }

    ".getMessage" - {

      "should return a single document with the given eori and id" - {

        "when there is a document in the collection with that eori and id" in {
          val mongoAesIE507MessagesDifferentEoriAndId: Seq[MongoAesIE507Message] =
            Seq.fill(2)(arbitrary[MongoAesIE507Message].sample).flatten

          val mongoAesIE507MessagesMatchingEoriAndId: Seq[MongoAesIE507Message] =
            Seq
              .fill(1)(
                arbitrary[MongoAesIE507Message]
                  .withEori(TestData.eoriNumber)
                  .withSubmissionId(TestData.submissionId)
                  .sample
              )
              .flatten

          val mongoAesIE507Messages: Seq[MongoAesIE507Message] =
            mongoAesIE507MessagesDifferentEoriAndId ++ mongoAesIE507MessagesMatchingEoriAndId

          repository.collection.insertMany(mongoAesIE507Messages).head().futureValue

          val result: MongoAesIE507Message =
            repository.getMessage(TestData.eoriNumber, TestData.submissionId).value.futureValue.value

          Seq(result) shouldBe mongoAesIE507MessagesMatchingEoriAndId
        }
      }

      "should return a MongoError" - {

        "when there are no documents in the collection with that eori and id" in {
          val mongoAesIE507MessagesDifferentEoriAndId: Seq[MongoAesIE507Message] =
            Seq.fill(3)(arbitrary[MongoAesIE507Message].sample).flatten

          repository.collection.insertMany(mongoAesIE507MessagesDifferentEoriAndId).head().futureValue

          val result: MongoError =
            repository.getMessage(TestData.eoriNumber, TestData.submissionId).value.futureValue.left.value

          result shouldBe MongoError.DocumentNotFound(
            s"No document found for EORI: ${TestData.eoriNumber.value} " +
              s"and submissionId: ${TestData.submissionId.value}"
          )
        }

        "when there are documents in the collection with that eori but different id" in {
          val mongoAesIE507MessagesDifferentId: Seq[MongoAesIE507Message] =
            Seq.fill(3)(arbitrary[MongoAesIE507Message].withEori(TestData.eoriNumber).sample).flatten

          repository.collection.insertMany(mongoAesIE507MessagesDifferentId).head().futureValue

          val result: MongoError =
            repository.getMessage(TestData.eoriNumber, TestData.submissionId).value.futureValue.left.value

          result shouldBe MongoError.DocumentNotFound(
            s"No document found for EORI: ${TestData.eoriNumber.value} " +
              s"and submissionId: ${TestData.submissionId.value}"
          )
        }

        "when there is a document in the collection with that id but different eori" in {
          val mongoAesIE507MessageDifferentEori: MongoAesIE507Message =
            arbitrary[MongoAesIE507Message]
              .withSubmissionId(TestData.submissionId)
              .sample
              .value

          repository.collection.insertOne(mongoAesIE507MessageDifferentEori).head().futureValue

          val result: MongoError =
            repository.getMessage(TestData.eoriNumber, TestData.submissionId).value.futureValue.left.value

          result shouldBe MongoError.DocumentNotFound(
            s"No document found for EORI: ${TestData.eoriNumber.value} " +
              s"and submissionId: ${TestData.submissionId.value}"
          )
        }
      }
    }

    ".cancel" - {

      "should set the document's ExportOperationType to Cancel" - {

        "when there is a document in the collection with that eori and submissionId" - {

          "and ExportOperationType is not Cancel" in {
            val mongoAesIE507MessagesDifferentEoriAndId: Seq[MongoAesIE507Message] =
              Seq.fill(2)(arbitrary[MongoAesIE507Message].sample).flatten

            val mongoAesIE507MessagesMatchingEoriAndId: Seq[MongoAesIE507Message] =
              Seq
                .fill(1)(
                  arbitrary[MongoAesIE507Message]
                    .withEori(TestData.eoriNumber)
                    .withSubmissionId(TestData.submissionId)
                    .withExportOperationType(ExportOperationType.Standard)
                    .sample
                )
                .flatten

            val mongoAesIE507Messages: Seq[MongoAesIE507Message] =
              mongoAesIE507MessagesDifferentEoriAndId ++ mongoAesIE507MessagesMatchingEoriAndId

            val mongoAesIE507MessagesMatchingEoriAndIdCancelled: Seq[MongoAesIE507Message] =
              mongoAesIE507MessagesMatchingEoriAndId.map(m =>
                m.copy(
                  exportOperation = m.exportOperation.copy(exportOperationType = ExportOperationType.Cancel),
                  updatedAt = TestData.instant
                )
              )

            repository.collection.insertMany(mongoAesIE507Messages).head().futureValue

            val updateStatus: SingleUpdateStatus =
              repository
                .cancel(
                  TestData.eoriNumber,
                  TestData.submissionId,
                  TestData.instant
                )
                .value
                .futureValue
                .value

            updateStatus shouldBe SingleUpdateStatus.Updated("cancel")

            val result: Seq[MongoAesIE507Message] =
              find(
                Filters.and(
                  Filters.eq("eoriNumber", TestData.eoriNumber.value),
                  Filters.eq("submissionId", TestData.submissionId.value.toString)
                )
              ).futureValue

            result shouldBe mongoAesIE507MessagesMatchingEoriAndIdCancelled
          }

          "and ExportOperationType is Cancel" in {
            val mongoAesIE507MessagesDifferentEoriAndId: Seq[MongoAesIE507Message] =
              Seq.fill(2)(arbitrary[MongoAesIE507Message].sample).flatten

            val mongoAesIE507MessagesMatchingEoriAndId: Seq[MongoAesIE507Message] =
              Seq
                .fill(1)(
                  arbitrary[MongoAesIE507Message]
                    .withSubmissionId(TestData.submissionId)
                    .withEori(TestData.eoriNumber)
                    .withExportOperationType(ExportOperationType.Cancel)
                    .sample
                )
                .flatten

            val mongoAesIE507Messages: Seq[MongoAesIE507Message] =
              mongoAesIE507MessagesDifferentEoriAndId ++ mongoAesIE507MessagesMatchingEoriAndId

            repository.collection.insertMany(mongoAesIE507Messages).head().futureValue

            val updateStatus: SingleUpdateStatus =
              repository
                .cancel(
                  TestData.eoriNumber,
                  TestData.submissionId,
                  TestData.instant
                )
                .value
                .futureValue
                .value

            updateStatus shouldBe SingleUpdateStatus.AlreadyUpToDate("cancel")

            val result: Seq[MongoAesIE507Message] =
              find(Filters.eq("submissionId", TestData.submissionId.value.toString)).futureValue

            result shouldBe mongoAesIE507MessagesMatchingEoriAndId
          }
        }
      }

      "should return a MongoError" - {

        "when there is no document in the collection with that submissionId" in {
          val mongoAesIE507MessagesDifferentId: Seq[MongoAesIE507Message] =
            Seq.fill(3)(arbitrary[MongoAesIE507Message].sample).flatten

          repository.collection.insertMany(mongoAesIE507MessagesDifferentId).head().futureValue

          val result: MongoError =
            repository
              .cancel(
                TestData.eoriNumber,
                TestData.submissionId,
                TestData.instant
              )
              .value
              .futureValue
              .left
              .value

          result shouldBe MongoError.DocumentNotFound(
            s"No document found for submissionId: ${TestData.submissionId.value}"
          )
        }
      }
    }

    ".updateNotification" - {

      "should update the matching notification event" - {

        "when there is a document in the collection with that eori and mrn" - {

          "where the most recent notification event with that correlation id is pending" - {

            "when there are no notification errors" in {
              val generatedMessage: MongoAesIE507Message =
                arbitrary[MongoAesIE507Message].sample.value

              val targetCorrelationId = "12345678-1234-1234-1234-12345678901"
              val otherCorrelationId  = "98765432-4321-4321-4321-10987654321"

              val targetEvent =
                generatedMessage.metadata.head.copy(
                  correlationId = targetCorrelationId,
                  isPending = true,
                  status = NotificationEventStatus.Awaiting,
                  errors = None
                )

              val otherEvent =
                generatedMessage.metadata.head.copy(
                  correlationId = otherCorrelationId
                )

              val message =
                generatedMessage.copy(
                  eoriNumber = TestData.eoriNumber,
                  exportOperation = generatedMessage.exportOperation.copy(
                    mrn = TestData.mrn
                  ),
                  metadata = NonEmptyList.of(targetEvent, otherEvent)
                )

              repository.collection.insertOne(message).head().futureValue

              val updatedAt =
                Instant.now().truncatedTo(ChronoUnit.MILLIS)

              val result =
                repository
                  .updateNotification(
                    TestData.eoriNumber,
                    TestData.mrn,
                    targetCorrelationId,
                    updatedAt,
                    NotificationEventStatus.Accepted,
                    None
                  )
                  .value
                  .futureValue
                  .value

              result shouldBe SingleUpdateStatus.Updated("updateNotification")

              val updatedTargetEvent: NotificationEvent =
                targetEvent.copy(
                  dateUpdated = updatedAt,
                  status = NotificationEventStatus.Accepted,
                  isPending = false,
                  errors = None
                )

              val updatedMessage =
                repository
                  .getMessageByNotification(
                    TestData.eoriNumber,
                    TestData.mrn,
                    targetCorrelationId
                  )
                  .value
                  .futureValue
                  .value

              val updatedTarget =
                updatedMessage.metadata.toList.find(_.correlationId == targetCorrelationId).value

              val unchangedOther =
                updatedMessage.metadata.toList.find(_.correlationId == otherCorrelationId).value

              updatedTarget  shouldBe updatedTargetEvent
              unchangedOther shouldBe otherEvent
            }

            "when there are notification errors" in {
              val generatedMessage =
                arbitrary[MongoAesIE507Message].sample.value

              val targetCorrelationId = "12345678-1234-1234-1234-12345678901"

              val targetEvent =
                generatedMessage.metadata.head.copy(
                  correlationId = targetCorrelationId,
                  errors = None,
                  isPending = true
                )

              val message =
                generatedMessage.copy(
                  eoriNumber = TestData.eoriNumber,
                  exportOperation = generatedMessage.exportOperation.copy(
                    mrn = TestData.mrn
                  ),
                  metadata = NonEmptyList.one(targetEvent)
                )

              repository.collection.insertOne(message).head().futureValue

              val notificationError =
                NotificationError(
                  code = "ERR001",
                  description = "Test error",
                  path = Some("/test/path"),
                  originalValue = Some("bad-value")
                )

              val updatedAt =
                Instant.now().truncatedTo(ChronoUnit.MILLIS)

              repository
                .updateNotification(
                  TestData.eoriNumber,
                  TestData.mrn,
                  targetCorrelationId,
                  updatedAt,
                  NotificationEventStatus.Rejected,
                  Some(NonEmptyList.one(notificationError))
                )
                .value
                .futureValue
                .value

              val updatedMessage =
                repository
                  .getMessageByNotification(
                    TestData.eoriNumber,
                    TestData.mrn,
                    targetCorrelationId
                  )
                  .value
                  .futureValue
                  .value

              val updatedTarget =
                updatedMessage.metadata.head

              updatedTarget.status shouldBe NotificationEventStatus.Rejected
              updatedTarget.errors shouldBe Some(
                NonEmptyList.one(notificationError)
              )
            }
          }
        }
      }

      "should return DocumentNotFound when no matching notification event exists" in {
        val result =
          repository
            .updateNotification(
              TestData.eoriNumber,
              TestData.mrn,
              "missing-correlation-id",
              Instant.now(),
              NotificationEventStatus.Accepted,
              None
            )
            .value
            .futureValue
            .left
            .value

        result shouldBe MongoError.DocumentNotFound(
          s"No document found for EORI: ${TestData.eoriNumber.value}, " +
            s"MRN: ${TestData.mrn.value}, with a notification event with correlationId: missing-correlation-id"
        )
      }
    }

    ".getMessageByNotification" - {

      "should return the document matching eori, mrn and correlationId" in {
        val generatedMessage: MongoAesIE507Message =
          arbitrary[MongoAesIE507Message].sample.value

        val matchingMessage: MongoAesIE507Message =
          generatedMessage.copy(
            eoriNumber = TestData.eoriNumber,
            exportOperation = generatedMessage.exportOperation.copy(
              mrn = TestData.mrn
            ),
            metadata = generatedMessage.metadata.map(
              _.copy(correlationId = TestData.correlationId)
            )
          )

        repository.collection.insertOne(matchingMessage).head().futureValue

        val result: MongoAesIE507Message =
          repository
            .getMessageByNotification(
              TestData.eoriNumber,
              TestData.mrn,
              TestData.correlationId
            )
            .value
            .futureValue
            .value

        result shouldBe matchingMessage
      }

      "should return DocumentNotFound when no matching document exists" in {

        val result: MongoError =
          repository
            .getMessageByNotification(
              TestData.eoriNumber,
              TestData.mrn,
              TestData.correlationId
            )
            .value
            .futureValue
            .left
            .value

        result shouldBe MongoError.DocumentNotFound(
          s"No document found for EORI: ${TestData.eoriNumber.value}, " +
            s"MRN: ${TestData.mrn.value}, with a notification event with correlationId: ${TestData.correlationId}"
        )
      }
    }

    ".pushNotificationAfterDiversion" - {

      "should push a notification event to the metadata array" - {

        "when there is a document in the collection with that eori and mrn" - {

          "where the most recent notification event with that correlation id is diverted" in {
            val mongoAesIE507Message: MongoAesIE507Message =
              arbitrary[MongoAesIE507Message]
                .withEori(TestData.eoriNumber)
                .withMrn(TestData.mrn)
                .sample
                .value

            val divertedNotificationEvent1: NotificationEvent =
              NotificationEvent(
                correlationId = TestData.correlationId,
                dateCreated = TestData.instant,
                dateUpdated = TestData.instant.plusMillis(1),
                isPending = false,
                status = NotificationEventStatus.Awaiting,
                errors = None
              )

            val mongoAesIE507MessageWithDivertedNotification: MongoAesIE507Message =
              mongoAesIE507Message.copy(metadata = NonEmptyList.one(divertedNotificationEvent1))

            repository.collection.insertOne(mongoAesIE507MessageWithDivertedNotification).head().futureValue

            val divertedNotificationEvent2: NotificationEvent =
              divertedNotificationEvent1.copy(
                dateCreated = TestData.instant.plusMillis(2),
                dateUpdated = TestData.instant.plusMillis(2)
              )

            val nonDivertedNotificationEvent: NotificationEvent =
              divertedNotificationEvent1.copy(
                dateCreated = TestData.instant.plusMillis(3),
                dateUpdated = TestData.instant.plusMillis(3),
                status = NotificationEventStatus.Accepted
              )

            val mongoAesIE507MessageAfterPush: MongoAesIE507Message =
              mongoAesIE507MessageWithDivertedNotification
                .copy(metadata =
                  NonEmptyList.of(
                    nonDivertedNotificationEvent,
                    divertedNotificationEvent2,
                    divertedNotificationEvent1
                  )
                )

            val divertedPushUpdateStatus: SingleUpdateStatus =
              repository
                .pushNotificationAfterDiversion(
                  TestData.eoriNumber,
                  TestData.mrn,
                  TestData.correlationId,
                  divertedNotificationEvent2
                )
                .value
                .futureValue
                .value

            val nonDivertedPushUpdateStatus: SingleUpdateStatus =
              repository
                .pushNotificationAfterDiversion(
                  TestData.eoriNumber,
                  TestData.mrn,
                  TestData.correlationId,
                  nonDivertedNotificationEvent
                )
                .value
                .futureValue
                .value

            divertedPushUpdateStatus    shouldBe SingleUpdateStatus.Updated("pushNotification")
            nonDivertedPushUpdateStatus shouldBe SingleUpdateStatus.Updated("pushNotification")

            val result: MongoAesIE507Message =
              repository
                .getMessageByNotification(
                  TestData.eoriNumber,
                  TestData.mrn,
                  TestData.correlationId
                )
                .value
                .futureValue
                .value

            result shouldBe mongoAesIE507MessageAfterPush
          }
        }
      }

      "should return a MongoError" - {

        "when there is a document in the collection with that eori and mrn" - {

          "where the most recent notification event with that correlation id is not diverted" in {
            val mongoAesIE507Message: MongoAesIE507Message =
              arbitrary[MongoAesIE507Message]
                .withEori(TestData.eoriNumber)
                .withMrn(TestData.mrn)
                .sample
                .value

            val divertedNotificationEvent: NotificationEvent =
              NotificationEvent(
                correlationId = TestData.correlationId,
                dateCreated = TestData.instant,
                dateUpdated = TestData.instant.plusMillis(1),
                isPending = false,
                status = NotificationEventStatus.Awaiting,
                errors = None
              )

            val nonDivertedNotificationEvent1: NotificationEvent =
              divertedNotificationEvent.copy(
                dateCreated = TestData.instant.plusMillis(2),
                dateUpdated = TestData.instant.plusMillis(2),
                status = NotificationEventStatus.Amended
              )

            val mongoAesIE507MessageWithDivertedNotification: MongoAesIE507Message =
              mongoAesIE507Message.copy(metadata =
                NonEmptyList.of(
                  nonDivertedNotificationEvent1,
                  divertedNotificationEvent
                )
              )

            repository.collection.insertOne(mongoAesIE507MessageWithDivertedNotification).head().futureValue

            val nonDivertedNotificationEvent2: NotificationEvent =
              divertedNotificationEvent.copy(
                dateCreated = TestData.instant.plusMillis(3),
                dateUpdated = TestData.instant.plusMillis(3),
                status = NotificationEventStatus.Accepted
              )

            val mongoAesIE507MessageAfterPush: MongoAesIE507Message =
              mongoAesIE507MessageWithDivertedNotification
                .copy(metadata =
                  NonEmptyList.of(
                    nonDivertedNotificationEvent1,
                    divertedNotificationEvent
                  )
                )

            val nonDivertedPushError: MongoError =
              repository
                .pushNotificationAfterDiversion(
                  TestData.eoriNumber,
                  TestData.mrn,
                  TestData.correlationId,
                  nonDivertedNotificationEvent2
                )
                .value
                .futureValue
                .left
                .value

            nonDivertedPushError shouldBe MongoError.DocumentNotFound(
              s"No document found for EORI: ${TestData.eoriNumber.value}, MRN: ${TestData.mrn.value}, where" +
                s" the most recent notification event with correlationId: ${TestData.correlationId} is diverted"
            )

            val result: MongoAesIE507Message =
              repository
                .getMessageByNotification(
                  TestData.eoriNumber,
                  TestData.mrn,
                  TestData.correlationId
                )
                .value
                .futureValue
                .value

            result shouldBe mongoAesIE507MessageAfterPush
          }
        }

        "when there is no document in the collection with that eori and mrn" in {
          val mongoAesIE507MessagesDifferentEoriAndMrn: Seq[MongoAesIE507Message] =
            Seq.fill(3)(arbitrary[MongoAesIE507Message].sample).flatten

          repository.collection.insertMany(mongoAesIE507MessagesDifferentEoriAndMrn).head().futureValue

          val notificationEvent: NotificationEvent =
            NotificationEvent(
              correlationId = TestData.correlationId,
              dateCreated = TestData.instant,
              dateUpdated = TestData.instant.plusMillis(1),
              isPending = false,
              status = NotificationEventStatus.Awaiting,
              errors = None
            )

          val result: MongoError =
            repository
              .pushNotificationAfterDiversion(
                TestData.eoriNumber,
                TestData.mrn,
                TestData.correlationId,
                notificationEvent
              )
              .value
              .futureValue
              .left
              .value

          result shouldBe MongoError.DocumentNotFound(
            s"No document found for EORI: ${TestData.eoriNumber.value}, MRN: ${TestData.mrn.value}, where" +
              s" the most recent notification event with correlationId: ${TestData.correlationId} is diverted"
          )
        }
      }
    }
  }
