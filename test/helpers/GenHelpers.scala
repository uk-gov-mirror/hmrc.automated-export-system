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

package helpers

import cats.data.NonEmptyList
import org.scalacheck.Gen
import uk.gov.hmrc.automatedexportsystem.models.IE507.aes.SubmissionId
import uk.gov.hmrc.automatedexportsystem.models.IE507.{EoriNumber, ExportOperationType, Mrn}
import uk.gov.hmrc.automatedexportsystem.models.mongo.write.MongoAesIE507Message
import uk.gov.hmrc.automatedexportsystem.models.notification.NotificationEventStatus

import java.time.Instant

trait GenHelpers:
  extension (mongoAesIE507MessageGen: Gen[MongoAesIE507Message])
    def withEori(eoriNumber: EoriNumber): Gen[MongoAesIE507Message] =
      mongoAesIE507MessageGen.map(_.copy(eoriNumber = eoriNumber))

    def withEoriAndStatus(
      eoriNumber: EoriNumber,
      status:     NotificationEventStatus
    ): Gen[MongoAesIE507Message] =
      mongoAesIE507MessageGen.map { message =>
        message.copy(
          eoriNumber = eoriNumber,
          metadata = message.metadata.copy(
            head = message.metadata.head.copy(status = status)
          )
        )
      }

    def withSubmissionId(submissionId: SubmissionId): Gen[MongoAesIE507Message] =
      mongoAesIE507MessageGen.map(_.copy(submissionId = submissionId))

    def withExportOperationType(exportOperationType: ExportOperationType): Gen[MongoAesIE507Message] =
      mongoAesIE507MessageGen.map(m => m.copy(exportOperation = m.exportOperation.copy(exportOperationType = exportOperationType)))

    def withUpdatedAt(updatedAt: Instant): Gen[MongoAesIE507Message] =
      mongoAesIE507MessageGen.map(_.copy(updatedAt = updatedAt))

    def withMrn(mrn: Mrn): Gen[MongoAesIE507Message] =
      mongoAesIE507MessageGen.map(m => m.copy(exportOperation = m.exportOperation.copy(mrn = mrn)))

object GenHelpers extends GenHelpers
