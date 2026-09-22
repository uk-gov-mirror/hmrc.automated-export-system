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

import cats.data.{EitherT, NonEmptyList}
import com.github.tomakehurst.wiremock.client.MappingBuilder
import com.github.tomakehurst.wiremock.client.WireMock.*
import helpers.EitherTFutureOps.toEitherTLeft
import helpers.XmlOps
import org.apache.pekko.util.ByteString
import org.mockito.ArgumentMatchers.any as mAny
import org.mockito.Mockito.when
import org.mongodb.scala.model.Filters
import play.api.inject.Binding
import play.api.mvc.{AnyContentAsEmpty, Result}
import play.api.test.Helpers.*
import play.api.test.{FakeRequest, Helpers}
import play.api.{Application, inject}
import uk.gov.hmrc.automatedexportsystem.connectors.EisConnector
import uk.gov.hmrc.automatedexportsystem.errors.{ConnectorError, MongoError}
import uk.gov.hmrc.automatedexportsystem.helpers.BaseISpec
import uk.gov.hmrc.automatedexportsystem.models.IE507.*
import uk.gov.hmrc.automatedexportsystem.models.IE507.aes.SubmissionId
import uk.gov.hmrc.automatedexportsystem.models.eis.{EisErrorResponse, EisIE507Request}
import uk.gov.hmrc.automatedexportsystem.models.http.{CustomHeaderNames, HttpHeader}
import uk.gov.hmrc.automatedexportsystem.models.mongo.SingleUpdateStatus
import uk.gov.hmrc.automatedexportsystem.models.mongo.write.MongoAesIE507Message
import uk.gov.hmrc.automatedexportsystem.models.notification.NotificationEventStatus.Awaiting
import uk.gov.hmrc.automatedexportsystem.models.notification.{NotificationEvent, NotificationEventStatus}
import uk.gov.hmrc.automatedexportsystem.models.responses.{SubmissionSummary, SubmissionSummaryList}
import uk.gov.hmrc.automatedexportsystem.repositories.{AesIE507Repository, AesIE507RepositoryImpl}
import uk.gov.hmrc.http.HeaderCarrier

import java.time.*
import java.util.UUID
import scala.concurrent.Future
import scala.xml.{Elem, NodeSeq}

class SubmissionControllerITSpec extends BaseISpec:
  trait Setup:
    val eori:            String        = "GB123456789000"
    val id1:             UUID          = UUID.fromString("6fb33641-6dc7-4a4f-adef-06238c13a317")
    val id2:             UUID          = UUID.fromString("4b10d823-4585-4f1e-bea5-d4bbe4605d6e")
    val instant:         Instant       = Instant.parse("2026-08-03T00:00:00.000Z")
    val dateTime:        LocalDateTime = LocalDateTime.parse("2026-08-03T00:00:00")
    val correlationId:   String        = "correlationId"
    val conversationId:  String        = "conversationId"
    val rfc1123DateTime: String        = "Mon, 3 Aug 2026 00:00:00 GMT"
    val bearerToken:     String        = "Bearer token"

    val correlationIdHeader:  HttpHeader.CorrelationId  = HttpHeader.CorrelationId(correlationId)
    val conversationIdHeader: HttpHeader.ConversationId = HttpHeader.ConversationId(conversationId)
    val authorizationHeader:  HttpHeader.Authorization  = HttpHeader.Authorization(bearerToken)
    val dateHeader:           HttpHeader.Date           = HttpHeader.Date(rfc1123DateTime)

    val authSuccessPayload: String =
      s"""{
        |  "allEnrolments": [
        |    {
        |      "key": "HMRC-CUS-ORG",
        |      "identifiers": [
        |        {
        |          "key": "EORINumber",
        |          "value": "$eori"
        |        }
        |      ],
        |      "state": "Activated"
        |    }
        |  ]
        |}""".stripMargin

    val mongoAesIE507Message1: MongoAesIE507Message =
      MongoAesIE507Message(
        submissionId = SubmissionId(id1),
        eoriNumber = EoriNumber(eori),
        createdAt = instant,
        updatedAt = instant,
        exportOperation = ExportOperation(
          exportOperationType = ExportOperationType.Standard,
          mrn = Mrn("mrn"),
          discrepanciesExist = DiscrepanciesExist(false),
          splitIndicator = SplitIndicator(true)
        ),
        customsOfficeOfExitActual = CustomsOfficeOfExitActual(
          referenceNumber = ReferenceNumber("referenceNumber")
        ),
        goodsShipment = Some(
          GoodsShipment(
            consignment = Consignment(
              modeOfTransportAtTheBorder = Some(ModeOfTransportAtTheBorder(1)),
              referenceNumberUCR = ReferenceNumberUcr("referenceNumberUcr"),
              parentUcrId = Some(ParentUcrId("parentUcrId")),
              transportEquipment = Some(
                NonEmptyList.one(
                  TransportEquipment(
                    sequenceNumber = Some(SequenceNumber(1)),
                    containerIdentificationNumber = Some(ContainerIdentificationNumber("1")),
                    numberOfSeals = Some(NumberOfSeals(1)),
                    seal = Some(
                      NonEmptyList.one(
                        Seal(
                          sequenceNumber = Some(SequenceNumber(1)),
                          sealIdentifier = Some(SealIdentifier("sealIdentifier"))
                        )
                      )
                    ),
                    goodsReference = Some(
                      NonEmptyList.one(
                        GoodsReference(
                          sequenceNumber = Some(SequenceNumber(1)),
                          declarationGoodsItemNumber = Some(DeclarationGoodsItemNumber(1))
                        )
                      )
                    )
                  )
                )
              ),
              locationOfGoods = LocationOfGoods(
                typeOfLocation = TypeOfLocation("typeOfLocation"),
                qualifierOfIdentification = QualifierOfIdentification("qualifierOfIdentification"),
                authorisationNumber = Some(AuthorisationNumber("authorisationNumber")),
                additionalIdentifier = Some(AdditionalIdentifier("additionalIdentifier")),
                unLocode = Some(UnLocode("unLocode"))
              ),
              activeBorderTransportMeans = Some(
                ActiveBorderTransportMeans(
                  typeOfIdentification = Some(TypeOfIdentification("typeOfIdentification")),
                  identificationNumber = Some(IdentificationNumber("identificationNumber")),
                  nationality = Some(Nationality("nationality"))
                )
              ),
              transportDocument = Some(
                NonEmptyList.one(
                  TransportDocument(
                    sequenceNumber = Some(SequenceNumber(1)),
                    transportDocumentType = Some(TransportDocumentType(1)),
                    referenceNumber = Some(ReferenceNumber("referenceNumber"))
                  )
                )
              )
            ),
            goodsItem = Some(
              NonEmptyList.one(
                GoodsItem(
                  referenceNumberUcr = Some(ReferenceNumberUcr("ducr")),
                  declarationGoodsItemNumber = Some(DeclarationGoodsItemNumber(1)),
                  commodity = Commodity(
                    goodsMeasure = GoodsMeasure(
                      grossMass = GrossMass(100.55),
                      netMass = NetMass(80.45)
                    )
                  ),
                  packaging = Some(
                    NonEmptyList.one(
                      Packaging(
                        sequenceNumber = Some(SequenceNumber(1)),
                        typeOfPackages = Some(TypeOfPackages("typeOfPackages")),
                        numberOfPackages = Some(NumberOfPackages(1)),
                        shippingMarks = Some(ShippingMarks("shippingMarks"))
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        metadata = NonEmptyList.one(
          NotificationEvent(
            correlationId = correlationId,
            dateCreated = instant,
            dateUpdated = instant,
            isPending = true,
            status = NotificationEventStatus.Awaiting,
            errors = None
          )
        )
      )

    val mongoAesIE507Message2: MongoAesIE507Message =
      MongoAesIE507Message(
        submissionId = SubmissionId(id2),
        eoriNumber = EoriNumber(eori),
        createdAt = instant,
        updatedAt = instant,
        exportOperation = ExportOperation(
          exportOperationType = ExportOperationType.Cancel,
          mrn = Mrn("26GB0000X6524786A9"),
          discrepanciesExist = DiscrepanciesExist(false),
          splitIndicator = SplitIndicator(true)
        ),
        customsOfficeOfExitActual = CustomsOfficeOfExitActual(
          referenceNumber = ReferenceNumber("IEARK100")
        ),
        goodsShipment = None,
        metadata = NonEmptyList.one(
          NotificationEvent(
            correlationId = correlationId,
            dateCreated = instant,
            dateUpdated = instant,
            isPending = true,
            status = NotificationEventStatus.Awaiting,
            errors = None
          )
        )
      )

    val submissionSummary1: SubmissionSummary =
      SubmissionSummary(
        submissionId = SubmissionId(id1),
        mrn = Mrn("mrn"),
        ducr = Some(ReferenceNumberUcr("referenceNumberUcr")),
        officeOfExitCode = ReferenceNumber("referenceNumber"),
        updatedAt = dateTime,
        status = Awaiting
      )

    val submissionSummary2: SubmissionSummary =
      SubmissionSummary(
        submissionId = SubmissionId(id2),
        mrn = Mrn("mrn"),
        ducr = None,
        officeOfExitCode = ReferenceNumber("referenceNumber"),
        updatedAt = dateTime,
        status = Awaiting
      )

    val submissionSummaryList: SubmissionSummaryList =
      SubmissionSummaryList(List(submissionSummary1, submissionSummary2))

    val aesIE507MessageAllOptionalsXml: Elem =
      <aes:Submission xmlns:aes="http://ecs.dgtaxud.ec">
        <submissionId>{id1}</submissionId>
        <ExportOperation>
          <type>1</type>
          <MRN>26GB0000X6524786A9</MRN>
          <discrepanciesExist>1</discrepanciesExist>
          <splitIndicator>0</splitIndicator>
        </ExportOperation>
        <CustomsOfficeOfExitActual>
          <referenceNumber>IEARK100</referenceNumber>
        </CustomsOfficeOfExitActual>
        <GoodsShipment>
          <Consignment>
            <modeOfTransportAtTheBorder>1</modeOfTransportAtTheBorder>
            <referenceNumberUCR>6GB536187624189-S458</referenceNumberUCR>
            <parentUCRID>GB/ABC-12345</parentUCRID>
            <TransportEquipment>
              <sequenceNumber>1</sequenceNumber>
              <containerIdentificationNumber>CONT1234567890123</containerIdentificationNumber>
              <numberOfSeals>2</numberOfSeals>
              <Seal>
                <sequenceNumber>1</sequenceNumber>
                <identifier>SEAL123</identifier>
              </Seal>
              <Seal>
                <sequenceNumber>2</sequenceNumber>
                <identifier>SEAL124</identifier>
              </Seal>
              <GoodsReference>
                <sequenceNumber>1</sequenceNumber>
                <declarationGoodsItemNumber>1</declarationGoodsItemNumber>
              </GoodsReference>
              <GoodsReference>
                <sequenceNumber>2</sequenceNumber>
                <declarationGoodsItemNumber>10</declarationGoodsItemNumber>
              </GoodsReference>
            </TransportEquipment>
            <LocationOfGoods>
              <typeOfLocation>A</typeOfLocation>
              <qualifierOfIdentification>B</qualifierOfIdentification>
              <authorisationNumber>AUTH12345</authorisationNumber>
              <additionalIdentifier>AD01</additionalIdentifier>
              <UNLocode>UNLOCODE123</UNLocode>
            </LocationOfGoods>
            <ActiveBorderTransportMeans>
              <typeOfIdentification>20</typeOfIdentification>
              <identificationNumber>IDNUMBER123</identificationNumber>
              <nationality>GB</nationality>
            </ActiveBorderTransportMeans>
            <TransportDocument>
              <sequenceNumber>1</sequenceNumber>
              <type>2</type>
              <referenceNumber>REF123</referenceNumber>
            </TransportDocument>
            <TransportDocument>
              <sequenceNumber>2</sequenceNumber>
              <type>2</type>
              <referenceNumber>REF124</referenceNumber>
            </TransportDocument>
          </Consignment>
          <GoodsItem>
            <declarationGoodsItemNumber>2</declarationGoodsItemNumber>
            <referenceNumberUCR>4AA09AZ(-//)</referenceNumberUCR>
            <Commodity>
              <GoodsMeasure>
                <grossMass>1000.500000</grossMass>
                <netMass>900.500000</netMass>
              </GoodsMeasure>
            </Commodity>
            <Packaging>
              <sequenceNumber>1</sequenceNumber>
              <typeOfPackages>PA</typeOfPackages>
              <numberOfPackages>10</numberOfPackages>
              <shippingMarks>MARKS123</shippingMarks>
            </Packaging>
            <Packaging>
              <sequenceNumber>2</sequenceNumber>
              <typeOfPackages>PA</typeOfPackages>
              <numberOfPackages>10</numberOfPackages>
              <shippingMarks>MARKS1234</shippingMarks>
            </Packaging>
          </GoodsItem>
          <GoodsItem>
            <declarationGoodsItemNumber>1</declarationGoodsItemNumber>
            <Commodity>
              <GoodsMeasure>
                <grossMass>1000.500000</grossMass>
                <netMass>900.500000</netMass>
              </GoodsMeasure>
            </Commodity>
            <Packaging>
              <sequenceNumber>1</sequenceNumber>
              <typeOfPackages>PA</typeOfPackages>
              <numberOfPackages>10</numberOfPackages>
              <shippingMarks>MARKS123</shippingMarks>
            </Packaging>
            <Packaging>
              <sequenceNumber>2</sequenceNumber>
              <typeOfPackages>PA</typeOfPackages>
              <numberOfPackages>10</numberOfPackages>
              <shippingMarks>MARKS1234</shippingMarks>
            </Packaging>
          </GoodsItem>
        </GoodsShipment>
      </aes:Submission>
    end aesIE507MessageAllOptionalsXml

    val eisIE507MessageAllOptionalsXml: Elem =
      <n:CC507C xmlns:n="http://ecs.dgtaxud.ec">
        <Header>
          <messageSender>{eori}</messageSender>
          <messageRecipient>NECA.XI</messageRecipient>
          <preparationDateAndTime>2026-08-03T00:00:00</preparationDateAndTime>
          <messageIdentification>{correlationId}</messageIdentification>
          <messageType>CC507C</messageType>
        </Header>
        <Body>
          <ExportOperation>
            <type>1</type>
            <MRN>26GB0000X6524786A9</MRN>
            <discrepanciesExist>1</discrepanciesExist>
            <splitIndicator>0</splitIndicator>
          </ExportOperation>
          <CustomsOfficeOfExitActual>
            <referenceNumber>IEARK100</referenceNumber>
          </CustomsOfficeOfExitActual>
          <GoodsShipment>
            <Consignment>
              <modeOfTransportAtTheBorder>1</modeOfTransportAtTheBorder>
              <referenceNumberUCR>6GB536187624189-S458</referenceNumberUCR>
              <parentUCRID>GB/ABC-12345</parentUCRID>
              <TransportEquipment>
                <sequenceNumber>1</sequenceNumber>
                <containerIdentificationNumber>CONT1234567890123</containerIdentificationNumber>
                <numberOfSeals>2</numberOfSeals>
                <Seal>
                  <sequenceNumber>1</sequenceNumber>
                  <identifier>SEAL123</identifier>
                </Seal>
                <Seal>
                  <sequenceNumber>2</sequenceNumber>
                  <identifier>SEAL124</identifier>
                </Seal>
                <GoodsReference>
                  <sequenceNumber>1</sequenceNumber>
                  <declarationGoodsItemNumber>1</declarationGoodsItemNumber>
                </GoodsReference>
                <GoodsReference>
                  <sequenceNumber>2</sequenceNumber>
                  <declarationGoodsItemNumber>10</declarationGoodsItemNumber>
                </GoodsReference>
              </TransportEquipment>
              <LocationOfGoods>
                <typeOfLocation>A</typeOfLocation>
                <qualifierOfIdentification>B</qualifierOfIdentification>
                <authorisationNumber>AUTH12345</authorisationNumber>
                <additionalIdentifier>AD01</additionalIdentifier>
                <UNLocode>UNLOCODE123</UNLocode>
              </LocationOfGoods>
              <ActiveBorderTransportMeans>
                <typeOfIdentification>20</typeOfIdentification>
                <identificationNumber>IDNUMBER123</identificationNumber>
                <nationality>GB</nationality>
              </ActiveBorderTransportMeans>
              <TransportDocument>
                <sequenceNumber>1</sequenceNumber>
                <type>2</type>
                <referenceNumber>REF123</referenceNumber>
              </TransportDocument>
              <TransportDocument>
                <sequenceNumber>2</sequenceNumber>
                <type>2</type>
                <referenceNumber>REF124</referenceNumber>
              </TransportDocument>
            </Consignment>
            <GoodsItem>
              <declarationGoodsItemNumber>2</declarationGoodsItemNumber>
              <referenceNumberUCR>4AA09AZ(-//)</referenceNumberUCR>
              <Commodity>
                <GoodsMeasure>
                  <grossMass>1000.500000</grossMass>
                  <netMass>900.500000</netMass>
                </GoodsMeasure>
              </Commodity>
              <Packaging>
                <sequenceNumber>1</sequenceNumber>
                <typeOfPackages>PA</typeOfPackages>
                <numberOfPackages>10</numberOfPackages>
                <shippingMarks>MARKS123</shippingMarks>
              </Packaging>
              <Packaging>
                <sequenceNumber>2</sequenceNumber>
                <typeOfPackages>PA</typeOfPackages>
                <numberOfPackages>10</numberOfPackages>
                <shippingMarks>MARKS1234</shippingMarks>
              </Packaging>
            </GoodsItem>
            <GoodsItem>
              <declarationGoodsItemNumber>1</declarationGoodsItemNumber>
              <Commodity>
                <GoodsMeasure>
                  <grossMass>1000.500000</grossMass>
                  <netMass>900.500000</netMass>
                </GoodsMeasure>
              </Commodity>
              <Packaging>
                <sequenceNumber>1</sequenceNumber>
                <typeOfPackages>PA</typeOfPackages>
                <numberOfPackages>10</numberOfPackages>
                <shippingMarks>MARKS123</shippingMarks>
              </Packaging>
              <Packaging>
                <sequenceNumber>2</sequenceNumber>
                <typeOfPackages>PA</typeOfPackages>
                <numberOfPackages>10</numberOfPackages>
                <shippingMarks>MARKS1234</shippingMarks>
              </Packaging>
            </GoodsItem>
          </GoodsShipment>
        </Body>
      </n:CC507C>
    end eisIE507MessageAllOptionalsXml

    val aesIE507MessageNoOptionalsXml: Elem =
      <aes:Submission xmlns:aes="http://ecs.dgtaxud.ec">
        <submissionId>{id2}</submissionId>
        <ExportOperation>
          <type>3</type>
          <MRN>26GB0000X6524786A9</MRN>
          <discrepanciesExist>0</discrepanciesExist>
          <splitIndicator>1</splitIndicator>
        </ExportOperation>
        <CustomsOfficeOfExitActual>
          <referenceNumber>IEARK100</referenceNumber>
        </CustomsOfficeOfExitActual>
      </aes:Submission>

    val eisIE507MessageNoOptionalsXml: Elem =
      <n:CC507C xmlns:n="http://ecs.dgtaxud.ec">
        <Header>
          <messageSender>{eori}</messageSender>
          <messageRecipient>NECA.XI</messageRecipient>
          <preparationDateAndTime>2026-08-03T00:00:00</preparationDateAndTime>
          <messageIdentification>{correlationId}</messageIdentification>
          <messageType>CC507C</messageType>
        </Header>
        <Body>
          <ExportOperation>
            <type>3</type>
            <MRN>26GB0000X6524786A9</MRN>
            <discrepanciesExist>0</discrepanciesExist>
            <splitIndicator>1</splitIndicator>
          </ExportOperation>
          <CustomsOfficeOfExitActual>
            <referenceNumber>IEARK100</referenceNumber>
          </CustomsOfficeOfExitActual>
        </Body>
      </n:CC507C>

    def eisPostRequestMappingBuilder(eisIE507MessageXml: Elem): MappingBuilder =
      post(urlEqualTo("/cds/aesIE507Request/v1"))
        .withHeader(CustomHeaderNames.X_CORRELATION_ID, equalTo(correlationId))
        .withHeader(CustomHeaderNames.X_CONVERSATION_ID, equalTo(conversationId))
        .withHeader(Helpers.X_FORWARDED_HOST, equalTo("automated-export-system"))
        .withHeader(CustomHeaderNames.X_MESSAGE_TYPE, equalTo("aesIE507Request"))
        .withHeader(Helpers.CONTENT_TYPE, equalTo(Helpers.XML))
        .withHeader(Helpers.ACCEPT, equalTo(Helpers.XML))
        .withHeader(Helpers.AUTHORIZATION, equalTo(bearerToken))
        .withHeader(Helpers.DATE, equalTo(rfc1123DateTime))
        .withRequestBody(equalToXml(eisIE507MessageXml.toString))
  end Setup

  object Setup extends Setup

  override def config: Map[String, Any] =
    super.config ++ Map("microservice.services.eis.bearerToken" -> Setup.bearerToken)

  override def bindingOverrides: Seq[Binding[_]] =
    super.bindingOverrides ++ Seq(
      inject.bind[Clock].toInstance(Clock.fixed(Setup.instant, ZoneOffset.UTC))
    )

  val aesIE507Repository: AesIE507RepositoryImpl = app.injector.instanceOf[AesIE507RepositoryImpl]

  override def beforeEach(): Unit =
    super.beforeEach()
    await(aesIE507Repository.collection.drop().head())

  "SubmissionController" - {

    "should handle an incoming POST request to the /message endpoint" - {

      "and return a 202 response" - {

        "when the request contains a valid AES IE507 XML body with all optional elements" - {

          "and the submission is successfully submitted to EIS" in new Setup {
            val requestXml: Elem = aesIE507MessageAllOptionalsXml

            stubFor(
              post(urlEqualTo("/auth/authorise"))
                .willReturn(
                  aResponse()
                    .withStatus(200)
                    .withHeader("Content-Type", "application/json")
                    .withBody(authSuccessPayload)
                )
            )

            stubFor(
              eisPostRequestMappingBuilder(eisIE507MessageAllOptionalsXml)
                .willReturn(
                  aResponse()
                    .withStatus(Helpers.NO_CONTENT)
                )
            )

            val request: FakeRequest[NodeSeq] =
              FakeRequest(Helpers.POST, "/automated-export-system/message")
                .withHeaders(
                  Helpers.AUTHORIZATION               -> "Bearer valid-token-123",
                  CustomHeaderNames.X_CORRELATION_ID  -> correlationId,
                  CustomHeaderNames.X_CONVERSATION_ID -> conversationId
                )
                .withBody(requestXml)

            val result: Future[Result] = Helpers.route(app, request).value

            Helpers.status(result)         shouldBe Helpers.ACCEPTED
            Helpers.contentType(result)    shouldBe None
            Helpers.contentAsBytes(result) shouldBe ByteString.empty
          }
        }

        "when the request contains an valid AES IE507 XML body without optional elements" - {

          "and the submission is successfully submitted to EIS" in new Setup {
            val requestXml: Elem = aesIE507MessageNoOptionalsXml

            stubFor(
              post(urlEqualTo("/auth/authorise"))
                .willReturn(
                  aResponse()
                    .withStatus(200)
                    .withHeader("Content-Type", "application/json")
                    .withBody(authSuccessPayload)
                )
            )

            stubFor(
              eisPostRequestMappingBuilder(eisIE507MessageNoOptionalsXml)
                .willReturn(
                  aResponse()
                    .withStatus(Helpers.NO_CONTENT)
                )
            )

            val request: FakeRequest[NodeSeq] =
              FakeRequest(Helpers.POST, "/automated-export-system/message")
                .withHeaders(
                  Helpers.AUTHORIZATION               -> "Bearer valid-token-123",
                  CustomHeaderNames.X_CORRELATION_ID  -> correlationId,
                  CustomHeaderNames.X_CONVERSATION_ID -> conversationId
                )
                .withBody(requestXml)

            val result: Future[Result] = Helpers.route(app, request).value

            Helpers.status(result)         shouldBe Helpers.ACCEPTED
            Helpers.contentType(result)    shouldBe None
            Helpers.contentAsBytes(result) shouldBe ByteString.empty
          }
        }
      }

      "and return a response with a status code specific to the EisErrorResponse received" - {

        "when the request contains a valid AES IE507 XML body" - {

          "and EIS returns a EisErrorResponse" - {

            "with a 400 errorCode" in new Setup {
              val requestXml: Elem = aesIE507MessageAllOptionalsXml

              stubFor(
                post(urlEqualTo("/auth/authorise"))
                  .willReturn(
                    aResponse()
                      .withStatus(200)
                      .withHeader("Content-Type", "application/json")
                      .withBody(authSuccessPayload)
                  )
              )

              val eisErrorResponseXml: Elem =
                <errorDetail xmlns="http://www.hmrc.gsi.gov.uk/eis">
                  <timestamp>{instant}</timestamp>
                  <correlationId>{correlationId}</correlationId>
                  <errorCode>{Helpers.BAD_REQUEST}</errorCode>
                  <errorMessage>errorMessage</errorMessage>
                  <source>source</source>
                  <sourceFaultDetail>
                    <detail>detail1</detail>
                    <detail>detail2</detail>
                    <detail>detail3</detail>
                  </sourceFaultDetail>
                </errorDetail>

              stubFor(
                eisPostRequestMappingBuilder(eisIE507MessageAllOptionalsXml)
                  .willReturn(
                    aResponse()
                      .withStatus(Helpers.BAD_REQUEST)
                      .withBody(eisErrorResponseXml.toString)
                  )
              )

              val request: FakeRequest[NodeSeq] =
                FakeRequest(Helpers.POST, "/automated-export-system/message")
                  .withHeaders(
                    Helpers.AUTHORIZATION               -> "Bearer valid-token-123",
                    CustomHeaderNames.X_CORRELATION_ID  -> correlationId,
                    CustomHeaderNames.X_CONVERSATION_ID -> conversationId
                  )
                  .withBody(requestXml)

              val result: Future[Result] = Helpers.route(app, request).value

              val resultContent: String = Helpers.contentAsString(result)
              val resultXml:     Elem   = XmlOps.loadXmlFromString(resultContent).value

              Helpers.status(result)      shouldBe Helpers.BAD_REQUEST
              Helpers.contentType(result) shouldBe Some(Helpers.XML)
              XmlOps.normalize(resultXml) shouldBe XmlOps.normalize(eisErrorResponseXml)
            }

            "with a 500 errorCode" in new Setup {
              val requestXml: Elem = aesIE507MessageAllOptionalsXml

              stubFor(
                post(urlEqualTo("/auth/authorise"))
                  .willReturn(
                    aResponse()
                      .withStatus(200)
                      .withHeader("Content-Type", "application/json")
                      .withBody(authSuccessPayload)
                  )
              )

              val eisErrorResponseXml: Elem =
                <errorDetail xmlns="http://www.hmrc.gsi.gov.uk/eis">
                  <timestamp>{instant}</timestamp>
                  <correlationId>{correlationId}</correlationId>
                  <errorCode>{Helpers.INTERNAL_SERVER_ERROR}</errorCode>
                  <errorMessage>errorMessage</errorMessage>
                  <source>source</source>
                  <sourceFaultDetail>
                    <detail>detail1</detail>
                    <detail>detail2</detail>
                    <detail>detail3</detail>
                  </sourceFaultDetail>
                </errorDetail>

              stubFor(
                eisPostRequestMappingBuilder(eisIE507MessageAllOptionalsXml)
                  .willReturn(
                    aResponse()
                      .withStatus(Helpers.INTERNAL_SERVER_ERROR)
                      .withBody(eisErrorResponseXml.toString)
                  )
              )

              val request: FakeRequest[NodeSeq] =
                FakeRequest(Helpers.POST, "/automated-export-system/message")
                  .withHeaders(
                    Helpers.AUTHORIZATION               -> "Bearer valid-token-123",
                    CustomHeaderNames.X_CORRELATION_ID  -> correlationId,
                    CustomHeaderNames.X_CONVERSATION_ID -> conversationId
                  )
                  .withBody(requestXml)

              val result: Future[Result] = Helpers.route(app, request).value

              val resultContent: String = Helpers.contentAsString(result)
              val resultXml:     Elem   = XmlOps.loadXmlFromString(resultContent).value

              Helpers.status(result)      shouldBe Helpers.INTERNAL_SERVER_ERROR
              Helpers.contentType(result) shouldBe Some(Helpers.XML)
              XmlOps.normalize(resultXml) shouldBe XmlOps.normalize(eisErrorResponseXml)
            }
          }
        }

      }

      "and return a 400 response" - {

        "when the request contains a valid XML body that doesn't pass AES IE507 request schema validation" - {

          "due to missing required elements" in new Setup {
            val requestXml: Elem = XmlOps.loadXmlFromPath("/testdata/aesIE507RequestInvalidMissingRequired.xml").value
            stubFor(
              post(urlEqualTo("/auth/authorise"))
                .willReturn(
                  aResponse()
                    .withStatus(200)
                    .withHeader("Content-Type", "application/json")
                    .withBody(authSuccessPayload)
                )
            )

            val request: FakeRequest[NodeSeq] = FakeRequest(Helpers.POST, "/automated-export-system/message")
              .withHeaders(
                Helpers.AUTHORIZATION -> "Bearer valid-token-123",
                "X-Session-ID"        -> "some-session-id"
              )
              .withBody(requestXml)

            val xmlFailedValidationErrorResponseXml: Elem =
              <errorResponse>
                <status>400</status>
                <code>BAD_REQUEST</code>
                <message>XML failed schema validation</message>
                <errors>
                  <error>
                    <line>5</line>
                    <column>33</column>
                    <message>cvc-complex-type.2.4.a: Invalid content was found starting with element 'discrepanciesExist'. One of '{{MRN}}' is expected.</message>
                  </error>
                  <error>
                    <line>14</line>
                    <column>34</column>
                    <message>cvc-complex-type.2.4.a: Invalid content was found starting with element 'LocationOfGoods'. One of '{{referenceNumberUCR}}' is expected.</message>
                  </error>
                  <error>
                    <line>16</line>
                    <column>35</column>
                    <message>cvc-complex-type.2.4.b: The content of element 'LocationOfGoods' is not complete. One of '{{qualifierOfIdentification}}' is expected.</message>
                  </error>
                  <error>
                    <line>32</line>
                    <column>34</column>
                    <message>cvc-complex-type.2.4.a: Invalid content was found starting with element 'netMass'. One of '{{grossMass}}' is expected.</message>
                  </error>
                </errors>
              </errorResponse>

            val result:        Future[Result] = Helpers.route(app, request).value
            val resultContent: String         = Helpers.contentAsString(result)
            val resultXml:     Elem           = XmlOps.loadXmlFromString(resultContent).value

            Helpers.status(result)      shouldBe Helpers.BAD_REQUEST
            Helpers.contentType(result) shouldBe Some(Helpers.XML)
            XmlOps.normalize(resultXml) shouldBe XmlOps.normalize(xmlFailedValidationErrorResponseXml)
          }

          "due to elements not matching the required patterns" in new Setup {
            val requestXml: Elem = XmlOps.loadXmlFromPath("/testdata/aesIE507RequestInvalidBadPatterns.xml").value

            stubFor(
              post(urlEqualTo("/auth/authorise"))
                .willReturn(
                  aResponse()
                    .withStatus(200)
                    .withHeader("Content-Type", "application/json")
                    .withBody(authSuccessPayload)
                )
            )

            val request: FakeRequest[NodeSeq] = FakeRequest(Helpers.POST, "/automated-export-system/message")
              .withHeaders(
                Helpers.AUTHORIZATION -> "Bearer valid-token-123",
                "X-Session-ID"        -> "some-session-id"
              )
              .withBody(requestXml)

            val xmlFailedValidationErrorResponseXml: Elem =
              <errorResponse>
                <status>400</status>
                <code>BAD_REQUEST</code>
                <message>XML failed schema validation</message>
                <errors>
                  <error>
                    <line>2</line>
                    <column>24</column>
                    <message>cvc-pattern-valid: Value '' is not facet-valid with respect to pattern '.{{1,36}}' for type 'UK_AlphaNumeric36Type'.</message>
                  </error>
                  <error>
                    <line>2</line>
                    <column>24</column>
                    <message>cvc-type.3.1.3: The value '' of element 'submissionId' is not valid.</message>
                  </error>
                  <error>
                    <line>4</line>
                    <column>20</column>
                    <message>cvc-pattern-valid: Value '' is not facet-valid with respect to pattern '[1-3]{{1}}' for type 'UK_OneToThreeType'.</message>
                  </error>
                  <error>
                    <line>4</line>
                    <column>20</column>
                    <message>cvc-type.3.1.3: The value '' of element 'type' is not valid.</message>
                  </error>
                  <error>
                    <line>5</line>
                    <column>19</column>
                    <message>cvc-pattern-valid: Value '' is not facet-valid with respect to pattern '([2][4-9]|[3-9][0-9])[A-Z]{{2}}[A-Z0-9]{{12}}[A-E][0-9]' for type 'UK_MRNType'.</message>
                  </error>
                  <error>
                    <line>5</line>
                    <column>19</column>
                    <message>cvc-type.3.1.3: The value '' of element 'MRN' is not valid.</message>
                  </error>
                  <error>
                    <line>6</line>
                    <column>34</column>
                    <message>cvc-enumeration-valid: Value '' is not facet-valid with respect to enumeration '[0, 1]'. It must be a value from the enumeration.</message>
                  </error>
                  <error>
                    <line>6</line>
                    <column>34</column>
                    <message>cvc-type.3.1.3: The value '' of element 'discrepanciesExist' is not valid.</message>
                  </error>
                  <error>
                    <line>7</line>
                    <column>30</column>
                    <message>cvc-enumeration-valid: Value '' is not facet-valid with respect to enumeration '[0, 1]'. It must be a value from the enumeration.</message>
                  </error>
                  <error>
                    <line>7</line>
                    <column>30</column>
                    <message>cvc-type.3.1.3: The value '' of element 'splitIndicator' is not valid.</message>
                  </error>
                  <error>
                    <line>10</line>
                    <column>31</column>
                    <message>cvc-pattern-valid: Value '' is not facet-valid with respect to pattern '[A-Z]{{2}}[A-Z0-9]{{6}}' for type 'UK_ReferenceNumberType'.</message>
                  </error>
                  <error>
                    <line>10</line>
                    <column>31</column>
                    <message>cvc-type.3.1.3: The value '' of element 'referenceNumber' is not valid.</message>
                  </error>
                </errors>
              </errorResponse>

            val result:        Future[Result] = Helpers.route(app, request).value
            val resultContent: String         = Helpers.contentAsString(result)
            val resultXml:     Elem           = XmlOps.loadXmlFromString(resultContent).value

            Helpers.status(result)      shouldBe Helpers.BAD_REQUEST
            Helpers.contentType(result) shouldBe Some(Helpers.XML)
            XmlOps.normalize(resultXml) shouldBe XmlOps.normalize(xmlFailedValidationErrorResponseXml)
          }
        }
      }

      "and return a 500 response" - {

        "when the request contains a valid XML body that passes AES IE507 request schema validation" - {

          "due to an unexpected error encountered when upserting the submission" in new Setup {
            val requestXml: Elem = aesIE507MessageNoOptionalsXml

            stubFor(
              post(urlEqualTo("/auth/authorise"))
                .willReturn(
                  aResponse()
                    .withStatus(200)
                    .withHeader("Content-Type", "application/json")
                    .withBody(authSuccessPayload)
                )
            )

            val aesIE507Repository: AesIE507Repository = mock[AesIE507Repository]

            when(
              aesIE507Repository.submit(
                mongoAesIE507Message2.copy(exportOperation =
                  mongoAesIE507Message2.exportOperation.copy(exportOperationType = ExportOperationType.Standard)
                )
              )
            )
              .thenReturn(
                MongoError
                  .UnexpectedError(
                    Exception("Unexpected error")
                  )
                  .toEitherTLeft[SingleUpdateStatus]
              )

            val errorMessage: String =
              s"Submission update/insert failed. EORI: $eori, submissionId: $id2"

            val app: Application =
              guiceApplicationBuilder
                .overrides(inject.bind[AesIE507Repository].toInstance(aesIE507Repository))
                .build()

            val submissionEisSubmitFailureXml: Elem =
              <errorResponse>
                <status>500</status>
                <code>INTERNAL_SERVER_ERROR</code>
                <message>{errorMessage}</message>
              </errorResponse>

            val request: FakeRequest[NodeSeq] =
              FakeRequest(Helpers.POST, "/automated-export-system/message")
                .withHeaders(
                  Helpers.AUTHORIZATION               -> "Bearer valid-token-123",
                  CustomHeaderNames.X_CORRELATION_ID  -> correlationId,
                  CustomHeaderNames.X_CONVERSATION_ID -> conversationId
                )
                .withBody(requestXml)

            Helpers.running(app) {
              val result:        Future[Result] = Helpers.route(app, request).value
              val resultContent: String         = Helpers.contentAsString(result)
              val resultXml:     Elem           = XmlOps.loadXmlFromString(resultContent).value

              Helpers.status(result)      shouldBe Helpers.INTERNAL_SERVER_ERROR
              Helpers.contentType(result) shouldBe Some(Helpers.XML)
              XmlOps.normalize(resultXml) shouldBe XmlOps.normalize(submissionEisSubmitFailureXml)
            }
          }

          "due to an unexpected error encountered when submitting the message to EIS" in new Setup {
            val requestXml: Elem = aesIE507MessageNoOptionalsXml

            stubFor(
              post(urlEqualTo("/auth/authorise"))
                .willReturn(
                  aResponse()
                    .withStatus(200)
                    .withHeader("Content-Type", "application/json")
                    .withBody(authSuccessPayload)
                )
            )

            val eisConnector: EisConnector = mock[EisConnector]

            when(eisConnector.submitMessage(mAny[EisIE507Request])(using mAny[HeaderCarrier]))
              .thenReturn(
                ConnectorError
                  .UnexpectedError(
                    Helpers.POST,
                    "http://localhost:6001/cds/aesIE507Request/v1",
                    Exception("Unexpected error")
                  )
                  .toEitherTLeft[Either[EisErrorResponse, Unit]]
              )

            val errorMessage: String =
              s"Failed to submit IE507 message to EIS. EORI: $eori, submissionId: $id2"

            val app: Application =
              guiceApplicationBuilder
                .overrides(inject.bind[EisConnector].toInstance(eisConnector))
                .build()

            val submissionEisSubmitFailureXml: Elem =
              <errorResponse>
                <status>500</status>
                <code>INTERNAL_SERVER_ERROR</code>
                <message>{errorMessage}</message>
              </errorResponse>

            val request: FakeRequest[NodeSeq] =
              FakeRequest(Helpers.POST, "/automated-export-system/message")
                .withHeaders(
                  Helpers.AUTHORIZATION               -> "Bearer valid-token-123",
                  CustomHeaderNames.X_CORRELATION_ID  -> correlationId,
                  CustomHeaderNames.X_CONVERSATION_ID -> conversationId
                )
                .withBody(requestXml)

            Helpers.running(app) {
              val result:        Future[Result] = Helpers.route(app, request).value
              val resultContent: String         = Helpers.contentAsString(result)
              val resultXml:     Elem           = XmlOps.loadXmlFromString(resultContent).value

              Helpers.status(result)      shouldBe Helpers.INTERNAL_SERVER_ERROR
              Helpers.contentType(result) shouldBe Some(Helpers.XML)
              XmlOps.normalize(resultXml) shouldBe XmlOps.normalize(submissionEisSubmitFailureXml)
            }
          }
        }
      }
    }

    "should handle an incoming GET request to the /submissions endpoint" - {

      "when the request contains a valid EORI" - {

        "and return a 200 response" - {

          "when there are submissions found with that EORI" in new Setup {
            stubFor(
              post(urlEqualTo("/auth/authorise"))
                .willReturn(
                  aResponse()
                    .withStatus(200)
                    .withHeader("Content-Type", "application/json")
                    .withBody(authSuccessPayload)
                )
            )

            await(aesIE507Repository.collection.insertMany(Seq(mongoAesIE507Message1, mongoAesIE507Message2)).head())

            val request: FakeRequest[AnyContentAsEmpty.type] =
              FakeRequest(Helpers.GET, "/automated-export-system/submissions")
                .withHeaders(
                  Helpers.AUTHORIZATION -> "Bearer valid-token-123",
                  "X-Session-ID"        -> "some-session-id"
                )

            val submissionSummaryListXml: Elem =
              <Submissions>
                <Submission>
                  <submissionId>6fb33641-6dc7-4a4f-adef-06238c13a317</submissionId>
                  <mrn>mrn</mrn>
                  <ducr>referenceNumberUcr</ducr>
                  <officeOfExitCode>referenceNumber</officeOfExitCode>
                  <updatedAt>2026-08-03T00:00:00</updatedAt>
                  <status>0</status>
                </Submission>
                <Submission>
                  <submissionId>4b10d823-4585-4f1e-bea5-d4bbe4605d6e</submissionId>
                  <mrn>26GB0000X6524786A9</mrn>
                  <officeOfExitCode>IEARK100</officeOfExitCode>
                  <updatedAt>2026-08-03T00:00:00</updatedAt>
                  <status>0</status>
                </Submission>
              </Submissions>

            val result:        Future[Result] = Helpers.route(app, request).value
            val resultContent: String         = Helpers.contentAsString(result)
            val resultXml:     Elem           = XmlOps.loadXmlFromString(resultContent).value

            Helpers.status(result)      shouldBe Helpers.OK
            Helpers.contentType(result) shouldBe Some(Helpers.XML)
            XmlOps.normalize(resultXml) shouldBe XmlOps.normalize(submissionSummaryListXml)
          }

          "when there are no submissions found with that EORI" in new Setup {
            stubFor(
              post(urlEqualTo("/auth/authorise"))
                .willReturn(
                  aResponse()
                    .withStatus(200)
                    .withHeader("Content-Type", "application/json")
                    .withBody(authSuccessPayload)
                )
            )

            val request: FakeRequest[AnyContentAsEmpty.type] =
              FakeRequest(Helpers.GET, "/automated-export-system/submissions")
                .withHeaders(
                  Helpers.AUTHORIZATION -> "Bearer valid-token-123",
                  "X-Session-ID"        -> "some-session-id"
                )

            val submissionSummaryListXml: Elem =
              <Submissions>
              </Submissions>

            val result:        Future[Result] = Helpers.route(app, request).value
            val resultContent: String         = Helpers.contentAsString(result)
            val resultXml:     Elem           = XmlOps.loadXmlFromString(resultContent).value

            Helpers.status(result)      shouldBe Helpers.OK
            Helpers.contentType(result) shouldBe Some(Helpers.XML)
            XmlOps.normalize(resultXml) shouldBe XmlOps.normalize(submissionSummaryListXml)
          }
        }

        "and return a 500 response" - {

          "when there is an unexpected error encountered while retrieving the submissions" in new Setup {
            val aesIE507Repository: AesIE507Repository = mock[AesIE507Repository]

            val app: Application = guiceApplicationBuilder
              .overrides(
                inject.bind[AesIE507Repository].toInstance(aesIE507Repository)
              )
              .build()

            stubFor(
              post(urlEqualTo("/auth/authorise"))
                .willReturn(
                  aResponse()
                    .withStatus(200)
                    .withHeader("Content-Type", "application/json")
                    .withBody(authSuccessPayload)
                )
            )

            val mongoUnexpectedError: MongoError = MongoError.UnexpectedError(Exception("Unexpected error"))

            when(aesIE507Repository.getMessages(EoriNumber(eori)))
              .thenReturn(EitherT(Future.successful(Left(mongoUnexpectedError))))

            val request: FakeRequest[AnyContentAsEmpty.type] =
              FakeRequest(Helpers.GET, "/automated-export-system/submissions")
                .withHeaders(
                  Helpers.AUTHORIZATION -> "Bearer valid-token-123",
                  "X-Session-ID"        -> "some-session-id"
                )

            val submissionRetrieveFailureXml: Elem =
              <errorResponse>
                  <status>500</status>
                  <code>INTERNAL_SERVER_ERROR</code>
                  <message>Submission retrieval failed. EORI: GB123456789000</message>
                </errorResponse>

            Helpers.running(app) {
              val result:        Future[Result] = Helpers.route(app, request).value
              val resultContent: String         = Helpers.contentAsString(result)
              val resultXml:     Elem           = XmlOps.loadXmlFromString(resultContent).value

              Helpers.status(result)      shouldBe Helpers.INTERNAL_SERVER_ERROR
              Helpers.contentType(result) shouldBe Some(Helpers.XML)
              XmlOps.normalize(resultXml) shouldBe XmlOps.normalize(submissionRetrieveFailureXml)
            }
          }
        }
      }
    }

    "should handle an incoming GET request to the /submission/:submissionId endpoint" - {

      "when the request contains a valid EORI" - {

        "and return a 200 response" - {

          "when there is a submission found with that EORI and given submissionId" in new Setup {
            stubFor(
              post(urlEqualTo("/auth/authorise"))
                .willReturn(
                  aResponse()
                    .withStatus(200)
                    .withHeader("Content-Type", "application/json")
                    .withBody(authSuccessPayload)
                )
            )

            await(aesIE507Repository.collection.insertMany(Seq(mongoAesIE507Message1, mongoAesIE507Message2)).head())

            val request: FakeRequest[AnyContentAsEmpty.type] =
              FakeRequest(Helpers.GET, s"/automated-export-system/submission/$id1")
                .withHeaders(Helpers.AUTHORIZATION -> "Bearer valid-token-123")

            val submissionXml: Elem =
              <Submission>
                <submissionId>{id1}</submissionId>
                <status>0</status>
                <ExportOperation>
                  <type>1</type>
                  <MRN>mrn</MRN>
                  <discrepanciesExist>0</discrepanciesExist>
                  <splitIndicator>1</splitIndicator>
                </ExportOperation>
                <CustomsOfficeOfExitActual>
                  <referenceNumber>referenceNumber</referenceNumber>
                </CustomsOfficeOfExitActual>
                <GoodsShipment>
                  <Consignment>
                    <modeOfTransportAtTheBorder>1</modeOfTransportAtTheBorder>
                    <referenceNumberUCR>referenceNumberUcr</referenceNumberUCR>
                    <parentUCRID>parentUcrId</parentUCRID>
                    <TransportEquipment>
                      <sequenceNumber>1</sequenceNumber>
                      <containerIdentificationNumber>1</containerIdentificationNumber>
                      <numberOfSeals>1</numberOfSeals>
                      <Seal>
                        <sequenceNumber>1</sequenceNumber>
                        <identifier>sealIdentifier</identifier>
                      </Seal>
                      <GoodsReference>
                        <sequenceNumber>1</sequenceNumber>
                        <declarationGoodsItemNumber>1</declarationGoodsItemNumber>
                      </GoodsReference>
                    </TransportEquipment>
                    <LocationOfGoods>
                      <typeOfLocation>typeOfLocation</typeOfLocation>
                      <qualifierOfIdentification>qualifierOfIdentification</qualifierOfIdentification>
                      <authorisationNumber>authorisationNumber</authorisationNumber>
                      <additionalIdentifier>additionalIdentifier</additionalIdentifier>
                      <UNLocode>unLocode</UNLocode>
                    </LocationOfGoods>
                    <ActiveBorderTransportMeans>
                      <typeOfIdentification>typeOfIdentification</typeOfIdentification>
                      <identificationNumber>identificationNumber</identificationNumber>
                      <nationality>nationality</nationality>
                    </ActiveBorderTransportMeans>
                    <TransportDocument>
                      <sequenceNumber>1</sequenceNumber>
                      <type>1</type>
                      <referenceNumber>referenceNumber</referenceNumber>
                    </TransportDocument>
                  </Consignment>
                  <GoodsItem>
                    <declarationGoodsItemNumber>1</declarationGoodsItemNumber>
                    <referenceNumberUCR>ducr</referenceNumberUCR>
                    <Commodity>
                      <GoodsMeasure>
                        <grossMass>100.55</grossMass>
                        <netMass>80.45</netMass>
                      </GoodsMeasure>
                    </Commodity>
                    <Packaging>
                      <sequenceNumber>1</sequenceNumber>
                      <typeOfPackages>typeOfPackages</typeOfPackages>
                      <numberOfPackages>1</numberOfPackages>
                      <shippingMarks>shippingMarks</shippingMarks>
                    </Packaging>
                  </GoodsItem>
                </GoodsShipment>
                <updatedAt>2026-08-03T00:00:00</updatedAt>
              </Submission>
            end submissionXml

            val result:        Future[Result] = Helpers.route(app, request).value
            val resultContent: String         = Helpers.contentAsString(result)
            val resultXml:     Elem           = XmlOps.loadXmlFromString(resultContent).value

            Helpers.status(result)      shouldBe Helpers.OK
            Helpers.contentType(result) shouldBe Some(Helpers.XML)
            XmlOps.normalize(resultXml) shouldBe XmlOps.normalize(submissionXml)
          }
        }

        "and return a 404 response" - {

          "when there is no submission found with that EORI and given submissionId" in new Setup {
            stubFor(
              post(urlEqualTo("/auth/authorise"))
                .willReturn(
                  aResponse()
                    .withStatus(200)
                    .withHeader("Content-Type", "application/json")
                    .withBody(authSuccessPayload)
                )
            )

            await(aesIE507Repository.collection.insertOne(mongoAesIE507Message2).head())

            val request: FakeRequest[AnyContentAsEmpty.type] =
              FakeRequest(Helpers.GET, s"/automated-export-system/submission/$id1")
                .withHeaders(Helpers.AUTHORIZATION -> "Bearer valid-token-123")

            val submissionNotFoundXml: Elem =
              <errorResponse>
                  <status>404</status>
                  <code>NOT_FOUND</code>
                  <message>Submission not found. EORI: {eori}, submissionId: {id1}</message>
                </errorResponse>

            val result:        Future[Result] = Helpers.route(app, request).value
            val resultContent: String         = Helpers.contentAsString(result)
            val resultXml:     Elem           = XmlOps.loadXmlFromString(resultContent).value

            Helpers.status(result)      shouldBe Helpers.NOT_FOUND
            Helpers.contentType(result) shouldBe Some(Helpers.XML)
            XmlOps.normalize(resultXml) shouldBe XmlOps.normalize(submissionNotFoundXml)
          }
        }

        "and return a 500 response" - {

          "when there is an unexpected error encountered while retrieving the submission" in new Setup {
            val aesIE507Repository: AesIE507Repository = mock[AesIE507Repository]

            stubFor(
              post(urlEqualTo("/auth/authorise"))
                .willReturn(
                  aResponse()
                    .withStatus(200)
                    .withHeader("Content-Type", "application/json")
                    .withBody(authSuccessPayload)
                )
            )

            val app: Application = guiceApplicationBuilder
              .overrides(
                inject.bind[AesIE507Repository].toInstance(aesIE507Repository)
              )
              .build()

            when(aesIE507Repository.getMessage(EoriNumber(eori), SubmissionId(id1)))
              .thenReturn(
                EitherT(
                  Future.successful(
                    Left(
                      MongoError.UnexpectedError(Exception("Unexpected error"))
                    )
                  )
                )
              )

            val request: FakeRequest[AnyContentAsEmpty.type] =
              FakeRequest(Helpers.GET, s"/automated-export-system/submission/$id1")
                .withHeaders(Helpers.AUTHORIZATION -> "Bearer valid-token-123")

            val submissionRetrieveFailureXml: Elem =
              <errorResponse>
                  <status>500</status>
                  <code>INTERNAL_SERVER_ERROR</code>
                  <message>Submission retrieval failed. EORI: {eori}, submissionId: {id1}</message>
                </errorResponse>

            Helpers.running(app) {
              val result:        Future[Result] = Helpers.route(app, request).value
              val resultContent: String         = Helpers.contentAsString(result)
              val resultXml:     Elem           = XmlOps.loadXmlFromString(resultContent).value

              Helpers.status(result)      shouldBe Helpers.INTERNAL_SERVER_ERROR
              Helpers.contentType(result) shouldBe Some(Helpers.XML)
              XmlOps.normalize(resultXml) shouldBe XmlOps.normalize(submissionRetrieveFailureXml)
            }
          }
        }
      }
    }

    "should handle an incoming GET request to the /cancel/:submissionId endpoint" - {

      "when the request contains a valid EORI" - {

        "and return a 204 response" - {

          "when there is a submission found with that EORI and submissionId" - {

            "and the submission is not cancelled yet" in new Setup {
              stubFor(
                post(urlEqualTo("/auth/authorise"))
                  .willReturn(
                    aResponse()
                      .withStatus(200)
                      .withHeader("Content-Type", "application/json")
                      .withBody(authSuccessPayload)
                  )
              )

              await(aesIE507Repository.collection.insertOne(mongoAesIE507Message1).head())

              val request: FakeRequest[AnyContentAsEmpty.type] =
                FakeRequest(Helpers.GET, s"/automated-export-system/cancel/$id1")
                  .withHeaders(Helpers.AUTHORIZATION -> "Bearer valid-token-123")

              val result: Future[Result] = Helpers.route(app, request).value

              Helpers.status(result)         shouldBe Helpers.NO_CONTENT
              Helpers.contentType(result)    shouldBe None
              Helpers.contentAsBytes(result) shouldBe ByteString.empty

              val cancelledMessage: MongoAesIE507Message =
                await(
                  aesIE507Repository.collection
                    .find(
                      Filters.and(
                        Filters.eq("eoriNumber", eori),
                        Filters.eq("submissionId", id1.toString)
                      )
                    )
                    .head()
                )

              cancelledMessage.exportOperation.exportOperationType shouldBe ExportOperationType.Cancel
            }

            "and the submission is already cancelled" in new Setup {
              stubFor(
                post(urlEqualTo("/auth/authorise"))
                  .willReturn(
                    aResponse()
                      .withStatus(200)
                      .withHeader("Content-Type", "application/json")
                      .withBody(authSuccessPayload)
                  )
              )

              await(aesIE507Repository.collection.insertOne(mongoAesIE507Message2).head())

              val request: FakeRequest[AnyContentAsEmpty.type] =
                FakeRequest(Helpers.GET, s"/automated-export-system/cancel/$id2")
                  .withHeaders(Helpers.AUTHORIZATION -> "Bearer valid-token-123")

              val result: Future[Result] = Helpers.route(app, request).value

              Helpers.status(result)         shouldBe Helpers.NO_CONTENT
              Helpers.contentType(result)    shouldBe None
              Helpers.contentAsBytes(result) shouldBe ByteString.empty

              val cancelledMessage: MongoAesIE507Message =
                await(
                  aesIE507Repository.collection
                    .find(
                      Filters.and(
                        Filters.eq("eoriNumber", eori),
                        Filters.eq("submissionId", id2.toString)
                      )
                    )
                    .head()
                )

              cancelledMessage.exportOperation.exportOperationType shouldBe ExportOperationType.Cancel
              cancelledMessage.updatedAt                           shouldBe mongoAesIE507Message2.updatedAt
            }
          }
        }

        "and return a 404 response" - {

          "when there is no submission found with that EORI and submissionId" in new Setup {
            stubFor(
              post(urlEqualTo("/auth/authorise"))
                .willReturn(
                  aResponse()
                    .withStatus(200)
                    .withHeader("Content-Type", "application/json")
                    .withBody(authSuccessPayload)
                )
            )

            await(aesIE507Repository.collection.insertOne(mongoAesIE507Message2).head())

            val request: FakeRequest[AnyContentAsEmpty.type] =
              FakeRequest(Helpers.GET, s"/automated-export-system/cancel/$id1")
                .withHeaders(Helpers.AUTHORIZATION -> "Bearer valid-token-123")

            val submissionNotFoundXml: Elem =
              <errorResponse>
                <status>404</status>
                <code>NOT_FOUND</code>
                <message>Submission not found. EORI: {eori}, submissionId: {id1}</message>
              </errorResponse>

            val result: Future[Result] = Helpers.route(app, request).value

            val resultContent: String = Helpers.contentAsString(result)
            val resultXml:     Elem   = XmlOps.loadXmlFromString(resultContent).value

            Helpers.status(result)      shouldBe Helpers.NOT_FOUND
            Helpers.contentType(result) shouldBe Some(Helpers.XML)
            XmlOps.normalize(resultXml) shouldBe XmlOps.normalize(submissionNotFoundXml)
          }
        }

        "and return a 500 response" - {

          "when there is an unexpected error encountered while updating the submission" in new Setup {
            val aesIE507Repository: AesIE507Repository = mock[AesIE507Repository]

            stubFor(
              post(urlEqualTo("/auth/authorise"))
                .willReturn(
                  aResponse()
                    .withStatus(200)
                    .withHeader("Content-Type", "application/json")
                    .withBody(authSuccessPayload)
                )
            )

            val app: Application = guiceApplicationBuilder
              .overrides(
                inject.bind[AesIE507Repository].toInstance(aesIE507Repository)
              )
              .build()

            when(aesIE507Repository.cancel(EoriNumber(eori), SubmissionId(id1), instant))
              .thenReturn(MongoError.WriteUnacknowledgedError.toEitherTLeft[SingleUpdateStatus])

            val request: FakeRequest[AnyContentAsEmpty.type] =
              FakeRequest(Helpers.GET, s"/automated-export-system/cancel/$id1")
                .withHeaders(Helpers.AUTHORIZATION -> "Bearer valid-token-123")

            val submissionUpdateFailureXml: Elem =
              <errorResponse>
                <status>500</status>
                <code>INTERNAL_SERVER_ERROR</code>
                <message>Submission update failed. EORI: {eori}, submissionId: {id1}</message>
              </errorResponse>

            Helpers.running(app) {
              val result:        Future[Result] = Helpers.route(app, request).value
              val resultContent: String         = Helpers.contentAsString(result)
              val resultXml:     Elem           = XmlOps.loadXmlFromString(resultContent).value

              Helpers.status(result)      shouldBe Helpers.INTERNAL_SERVER_ERROR
              Helpers.contentType(result) shouldBe Some(Helpers.XML)
              XmlOps.normalize(resultXml) shouldBe XmlOps.normalize(submissionUpdateFailureXml)
            }
          }
        }
      }
    }
  }
