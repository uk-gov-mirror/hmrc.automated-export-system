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

package uk.gov.hmrc.automatedexportsystem.controllers

import cats.data.NonEmptyList
import org.scalacheck.Arbitrary.arbitrary
import play.api.test.Helpers.*
import play.api.test.{FakeRequest, Helpers}
import uk.gov.hmrc.automatedexportsystem.generators.MongoAesIE507MessageGenerator
import uk.gov.hmrc.automatedexportsystem.helpers.BaseISpec
import uk.gov.hmrc.automatedexportsystem.models.IE507.{EoriNumber, ExportOperationType, Mrn}
import uk.gov.hmrc.automatedexportsystem.models.mongo.write.MongoAesIE507Message
import uk.gov.hmrc.automatedexportsystem.models.notification.NotificationEventStatus
import uk.gov.hmrc.automatedexportsystem.repositories.AesIE507RepositoryImpl

import scala.xml.{Elem, XML as Xml}

class NotificationControllerISpec extends BaseISpec with MongoAesIE507MessageGenerator:
  object TestData:
    val endpoint = "/automated-export-system/notification"

    val correlationId =
      "8f3c2a19-7d2b-4b74-a9f0-123456789012"

    val eoriNumber =
      EoriNumber("GB123456789000")

    val mrn =
      Mrn("25GB1234567890ABCDE")

    val validPayload: Elem =
      <notification>
        <correlationId>
          {correlationId}
        </correlationId>
        <eori>
          {eoriNumber.value}
        </eori>
        <mrn>
          {mrn.value}
        </mrn>
        <dateCreated>2026-08-12T10:15:30</dateCreated>
        <status>1</status>
      </notification>

    val invalidPayload =
      """<not-notification>
        |      <status>1</status>
        |    </notification>""".stripMargin

    val invalidXmlPayload =
      <notification>
        <status>1</status>
      </notification>
  end TestData

  val aesIE507Repository: AesIE507RepositoryImpl =
    app.injector.instanceOf[AesIE507RepositoryImpl]

  override def beforeEach(): Unit =
    super.beforeEach()
    await(aesIE507Repository.collection.drop().head())

  "POST /notification" - {

    "return 204 when authorization header is valid and payload is valid" in {

      val generatedMessage =
        arbitrary[MongoAesIE507Message].sample.value

      val notificationEvent =
        generatedMessage.metadata.head.copy(
          correlationId = TestData.correlationId,
          isPending = true,
          status = NotificationEventStatus.Awaiting,
          errors = None
        )

      val message =
        generatedMessage.copy(
          eoriNumber = TestData.eoriNumber,
          exportOperation = generatedMessage.exportOperation.copy(
            exportOperationType = ExportOperationType.Standard,
            mrn = TestData.mrn
          ),
          metadata = NonEmptyList.one(notificationEvent)
        )

      await(
        aesIE507Repository.collection
          .insertOne(message)
          .head()
      )

      val request = FakeRequest(Helpers.POST, TestData.endpoint)
        .withHeaders("Authorization" -> "some-token")
        .withXmlBody(TestData.validPayload)

      val result = Helpers.route(app, request).value

      Helpers.status(result) shouldBe Helpers.NO_CONTENT
    }

    "return 401 when authorization header is invalid" in {
      val request = FakeRequest(Helpers.POST, TestData.endpoint)
        .withHeaders("Authorization" -> "invalid-token")
        .withXmlBody(TestData.validPayload)

      val result = Helpers.route(app, request).value
      Helpers.status(result)      shouldBe Helpers.UNAUTHORIZED
      Helpers.contentType(result) shouldBe Some("application/xml")
      val resultXml = Xml.loadString(Helpers.contentAsString(result))
      (resultXml \ "code").text.trim shouldBe "UNAUTHORIZED"
    }

    "return 415 when authorization header is valid and payload is missing" in {
      val request = FakeRequest(Helpers.POST, TestData.endpoint)
        .withHeaders("Authorization" -> "some-token")

      val result = Helpers.route(app, request).value
      Helpers.status(result)      shouldBe Helpers.UNSUPPORTED_MEDIA_TYPE
      Helpers.contentType(result) shouldBe Some("application/xml")
      val resultXml = Xml.loadString(Helpers.contentAsString(result))
      (resultXml \ "code").text shouldBe "UNSUPPORTED_MEDIA_TYPE"
    }

    "return 422 when authorization header is valid and payload is invalid" in {
      val request = FakeRequest(Helpers.POST, TestData.endpoint)
        .withHeaders("Authorization" -> "some-token")
        .withBody(TestData.invalidPayload)

      val result = Helpers.route(app, request).value
      Helpers.status(result)      shouldBe Helpers.UNSUPPORTED_MEDIA_TYPE
      Helpers.contentType(result) shouldBe Some("application/xml")
      val resultXml = Xml.loadString(Helpers.contentAsString(result))
      (resultXml \ "code").text shouldBe "UNSUPPORTED_MEDIA_TYPE"
    }

    "return 422 when authorization header is valid and payload is invalid xml" in {
      val request = FakeRequest(Helpers.POST, TestData.endpoint)
        .withHeaders("Authorization" -> "some-token")
        .withBody(TestData.invalidXmlPayload)

      val result = Helpers.route(app, request).value
      Helpers.status(result)      shouldBe Helpers.UNPROCESSABLE_ENTITY
      Helpers.contentType(result) shouldBe Some("application/xml")
      val resultXml = Xml.loadString(Helpers.contentAsString(result))
      (resultXml \ "code").text shouldBe "UNPROCESSABLE_ENTITY"
    }
  }
