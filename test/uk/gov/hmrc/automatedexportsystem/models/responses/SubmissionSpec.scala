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

package uk.gov.hmrc.automatedexportsystem.models.responses

import cats.data.NonEmptyList
import helpers.XmlOps
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import uk.gov.hmrc.automatedexportsystem.models.IE507.*
import uk.gov.hmrc.automatedexportsystem.models.IE507.aes.SubmissionId
import uk.gov.hmrc.automatedexportsystem.models.mongo.write.MongoAesIE507Message
import uk.gov.hmrc.automatedexportsystem.models.notification.{NotificationError, NotificationEvent, NotificationEventStatus}
import uk.gov.hmrc.automatedexportsystem.xml.RootedXmlWriter.toXmlRoot

import java.time.{Instant, LocalDateTime}
import java.util.UUID
import scala.xml.Elem

class SubmissionSpec extends AnyFreeSpecLike, Matchers:
  object TestData:
    val id:            UUID          = UUID.fromString("6fb33641-6dc7-4a4f-adef-06238c13a317")
    val eoriNumber:    EoriNumber    = EoriNumber("eoriNumber")
    val correlationId: String        = "correlationId"
    val dateTime:      LocalDateTime = LocalDateTime.parse("2026-08-11T00:00:00")
    val instant:       Instant       = Instant.parse("2026-08-11T00:00:00Z")

    val notificationError1: NotificationError =
      NotificationError(
        "CODE_1",
        "description",
        Some("path"),
        Some("originalValue1")
      )

    val notificationError2: NotificationError =
      NotificationError(
        "CODE_2",
        "description",
        Some("path"),
        Some("originalValue2")
      )

    val submission: Submission =
      Submission(
        submissionId = SubmissionId(id),
        status = NotificationEventStatus.Rejected,
        exportOperation = ExportOperation(
          exportOperationType = ExportOperationType.Standard,
          mrn = Mrn("mrn"),
          discrepanciesExist = DiscrepanciesExist(true),
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
                NonEmptyList.of(
                  TransportEquipment(
                    sequenceNumber = Some(SequenceNumber(1)),
                    containerIdentificationNumber = Some(ContainerIdentificationNumber("1")),
                    numberOfSeals = Some(NumberOfSeals(1)),
                    seal = Some(
                      NonEmptyList.one(
                        Seal(
                          sequenceNumber = Some(SequenceNumber(1)),
                          sealIdentifier = Some(SealIdentifier("sealIdentifier1"))
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
                  ),
                  TransportEquipment(
                    sequenceNumber = Some(SequenceNumber(2)),
                    containerIdentificationNumber = Some(ContainerIdentificationNumber("2")),
                    numberOfSeals = Some(NumberOfSeals(1)),
                    seal = Some(
                      NonEmptyList.one(
                        Seal(
                          sequenceNumber = Some(SequenceNumber(2)),
                          sealIdentifier = Some(SealIdentifier("sealIdentifier2"))
                        )
                      )
                    ),
                    goodsReference = Some(
                      NonEmptyList.one(
                        GoodsReference(
                          sequenceNumber = Some(SequenceNumber(2)),
                          declarationGoodsItemNumber = Some(DeclarationGoodsItemNumber(2))
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
                NonEmptyList.of(
                  TransportDocument(
                    sequenceNumber = Some(SequenceNumber(1)),
                    transportDocumentType = Some(TransportDocumentType(1)),
                    referenceNumber = Some(ReferenceNumber("referenceNumber1"))
                  ),
                  TransportDocument(
                    sequenceNumber = Some(SequenceNumber(2)),
                    transportDocumentType = Some(TransportDocumentType(2)),
                    referenceNumber = Some(ReferenceNumber("referenceNumber2"))
                  )
                )
              )
            ),
            goodsItem = Some(
              NonEmptyList.of(
                GoodsItem(
                  declarationGoodsItemNumber = Some(DeclarationGoodsItemNumber(1)),
                  commodity = Commodity(
                    goodsMeasure = GoodsMeasure(
                      grossMass = GrossMass(100.55),
                      netMass = NetMass(80.45)
                    )
                  ),
                  referenceNumberUcr = Some(ReferenceNumberUcr("referenceNumberUcr")),
                  packaging = Some(
                    NonEmptyList.of(
                      Packaging(
                        sequenceNumber = Some(SequenceNumber(1)),
                        typeOfPackages = Some(TypeOfPackages("typeOfPackages")),
                        numberOfPackages = Some(NumberOfPackages(1)),
                        shippingMarks = Some(ShippingMarks("shippingMarks"))
                      ),
                      Packaging(
                        sequenceNumber = Some(SequenceNumber(2)),
                        typeOfPackages = Some(TypeOfPackages("typeOfPackages")),
                        numberOfPackages = Some(NumberOfPackages(1)),
                        shippingMarks = Some(ShippingMarks("shippingMarks"))
                      )
                    )
                  )
                ),
                GoodsItem(
                  declarationGoodsItemNumber = Some(DeclarationGoodsItemNumber(2)),
                  commodity = Commodity(
                    goodsMeasure = GoodsMeasure(
                      grossMass = GrossMass(100.55),
                      netMass = NetMass(80.45)
                    )
                  ),
                  referenceNumberUcr = Some(ReferenceNumberUcr("referenceNumberUcr")),
                  packaging = Some(
                    NonEmptyList.of(
                      Packaging(
                        sequenceNumber = Some(SequenceNumber(3)),
                        typeOfPackages = Some(TypeOfPackages("typeOfPackages")),
                        numberOfPackages = Some(NumberOfPackages(1)),
                        shippingMarks = Some(ShippingMarks("shippingMarks"))
                      ),
                      Packaging(
                        sequenceNumber = Some(SequenceNumber(4)),
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
        updatedAt = dateTime,
        metadata = Some(NonEmptyList.of(notificationError1, notificationError2))
      )
    end submission

    val submissionNoNonRootOptionals: Submission =
      Submission(
        submissionId = SubmissionId(id),
        status = NotificationEventStatus.Awaiting,
        exportOperation = ExportOperation(
          exportOperationType = ExportOperationType.Standard,
          mrn = Mrn("mrn"),
          discrepanciesExist = DiscrepanciesExist(true),
          splitIndicator = SplitIndicator(true)
        ),
        customsOfficeOfExitActual = CustomsOfficeOfExitActual(
          referenceNumber = ReferenceNumber("referenceNumber")
        ),
        goodsShipment = Some(
          GoodsShipment(
            consignment = Consignment(
              modeOfTransportAtTheBorder = None,
              referenceNumberUCR = ReferenceNumberUcr("referenceNumberUcr"),
              parentUcrId = None,
              transportEquipment = Some(
                NonEmptyList.of(
                  TransportEquipment(
                    sequenceNumber = None,
                    containerIdentificationNumber = None,
                    numberOfSeals = None,
                    seal = Some(
                      NonEmptyList.one(
                        Seal(
                          sequenceNumber = None,
                          sealIdentifier = None
                        )
                      )
                    ),
                    goodsReference = Some(
                      NonEmptyList.one(
                        GoodsReference(
                          sequenceNumber = None,
                          declarationGoodsItemNumber = None
                        )
                      )
                    )
                  ),
                  TransportEquipment(
                    sequenceNumber = None,
                    containerIdentificationNumber = None,
                    numberOfSeals = None,
                    seal = Some(
                      NonEmptyList.one(
                        Seal(
                          sequenceNumber = None,
                          sealIdentifier = None
                        )
                      )
                    ),
                    goodsReference = Some(
                      NonEmptyList.one(
                        GoodsReference(
                          sequenceNumber = None,
                          declarationGoodsItemNumber = None
                        )
                      )
                    )
                  )
                )
              ),
              locationOfGoods = LocationOfGoods(
                typeOfLocation = TypeOfLocation("typeOfLocation"),
                qualifierOfIdentification = QualifierOfIdentification("qualifierOfIdentification"),
                authorisationNumber = None,
                additionalIdentifier = None,
                unLocode = None
              ),
              activeBorderTransportMeans = Some(
                ActiveBorderTransportMeans(
                  typeOfIdentification = None,
                  identificationNumber = None,
                  nationality = None
                )
              ),
              transportDocument = Some(
                NonEmptyList.of(
                  TransportDocument(
                    sequenceNumber = None,
                    transportDocumentType = None,
                    referenceNumber = None
                  ),
                  TransportDocument(
                    sequenceNumber = None,
                    transportDocumentType = None,
                    referenceNumber = None
                  )
                )
              )
            ),
            goodsItem = Some(
              NonEmptyList.of(
                GoodsItem(
                  declarationGoodsItemNumber = None,
                  commodity = Commodity(
                    goodsMeasure = GoodsMeasure(
                      grossMass = GrossMass(100.55),
                      netMass = NetMass(80.45)
                    )
                  ),
                  referenceNumberUcr = None,
                  packaging = Some(
                    NonEmptyList.of(
                      Packaging(
                        sequenceNumber = None,
                        typeOfPackages = None,
                        numberOfPackages = None,
                        shippingMarks = None
                      ),
                      Packaging(
                        sequenceNumber = None,
                        typeOfPackages = None,
                        numberOfPackages = None,
                        shippingMarks = None
                      )
                    )
                  )
                ),
                GoodsItem(
                  declarationGoodsItemNumber = None,
                  commodity = Commodity(
                    goodsMeasure = GoodsMeasure(
                      grossMass = GrossMass(100.55),
                      netMass = NetMass(80.45)
                    )
                  ),
                  referenceNumberUcr = None,
                  packaging = Some(
                    NonEmptyList.of(
                      Packaging(
                        sequenceNumber = None,
                        typeOfPackages = None,
                        numberOfPackages = None,
                        shippingMarks = None
                      ),
                      Packaging(
                        sequenceNumber = None,
                        typeOfPackages = None,
                        numberOfPackages = None,
                        shippingMarks = None
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        updatedAt = dateTime,
        metadata = None
      )
    end submissionNoNonRootOptionals

    val submissionNoGoodsShipmentChildrenOptionals: Submission =
      Submission(
        submissionId = SubmissionId(id),
        status = NotificationEventStatus.Accepted,
        exportOperation = ExportOperation(
          exportOperationType = ExportOperationType.Standard,
          mrn = Mrn("mrn"),
          discrepanciesExist = DiscrepanciesExist(true),
          splitIndicator = SplitIndicator(true)
        ),
        customsOfficeOfExitActual = CustomsOfficeOfExitActual(
          referenceNumber = ReferenceNumber("referenceNumber")
        ),
        goodsShipment = Some(
          GoodsShipment(
            consignment = Consignment(
              modeOfTransportAtTheBorder = None,
              referenceNumberUCR = ReferenceNumberUcr("referenceNumberUcr"),
              parentUcrId = None,
              transportEquipment = None,
              locationOfGoods = LocationOfGoods(
                typeOfLocation = TypeOfLocation("typeOfLocation"),
                qualifierOfIdentification = QualifierOfIdentification("qualifierOfIdentification"),
                authorisationNumber = None,
                additionalIdentifier = None,
                unLocode = None
              ),
              activeBorderTransportMeans = None,
              transportDocument = None
            ),
            goodsItem = None
          )
        ),
        updatedAt = dateTime,
        metadata = None
      )
    end submissionNoGoodsShipmentChildrenOptionals

    val submissionNoGoodsShipment: Submission =
      Submission(
        submissionId = SubmissionId(id),
        status = NotificationEventStatus.Cancelled,
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
        updatedAt = dateTime,
        metadata = None
      )
    end submissionNoGoodsShipment

    val notificationEvent1: NotificationEvent =
      NotificationEvent(
        correlationId = correlationId,
        dateCreated = instant,
        dateUpdated = instant,
        isPending = false,
        status = NotificationEventStatus.Awaiting,
        errors = None
      )

    val notificationEvent2: NotificationEvent =
      notificationEvent1.copy(
        dateUpdated = instant.plusMillis(1),
        status = NotificationEventStatus.Accepted
      )

    val notificationEvent3: NotificationEvent =
      notificationEvent1.copy(
        dateCreated = instant.plusMillis(2),
        status = NotificationEventStatus.Rejected,
        errors = Some(NonEmptyList.one(notificationError1))
      )

    val notificationEvent4: NotificationEvent =
      notificationEvent3.copy(
        dateUpdated = instant.plusMillis(3),
        errors = notificationEvent3.errors.map(errors => errors :+ notificationError2)
      )

    val mongoAesIE507MessageOneEvent: MongoAesIE507Message =
      MongoAesIE507Message(
        submissionId = SubmissionId(id),
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
        metadata = NonEmptyList.one(notificationEvent1)
      )

    val mongoAesIE507MessageMultipleEvents: MongoAesIE507Message =
      mongoAesIE507MessageOneEvent.copy(metadata =
        NonEmptyList.of(
          notificationEvent3,
          notificationEvent2,
          notificationEvent4,
          notificationEvent1
        )
      )
  end TestData

  "Submission" - {

    "should be able to be serialized to XML" - {

      "when all fields are present, and lists are populated with more than one element" in {
        val xml: Elem =
          <Submission>
            <submissionId>{TestData.id}</submissionId>
            <status>4</status>
            <ExportOperation>
              <type>1</type>
              <MRN>mrn</MRN>
              <discrepanciesExist>1</discrepanciesExist>
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
                    <identifier>sealIdentifier1</identifier>
                  </Seal>
                  <GoodsReference>
                    <sequenceNumber>1</sequenceNumber>
                    <declarationGoodsItemNumber>1</declarationGoodsItemNumber>
                  </GoodsReference>
                </TransportEquipment>
                <TransportEquipment>
                  <sequenceNumber>2</sequenceNumber>
                  <containerIdentificationNumber>2</containerIdentificationNumber>
                  <numberOfSeals>1</numberOfSeals>
                  <Seal>
                    <sequenceNumber>2</sequenceNumber>
                    <identifier>sealIdentifier2</identifier>
                  </Seal>
                  <GoodsReference>
                    <sequenceNumber>2</sequenceNumber>
                    <declarationGoodsItemNumber>2</declarationGoodsItemNumber>
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
                  <referenceNumber>referenceNumber1</referenceNumber>
                </TransportDocument>
                <TransportDocument>
                  <sequenceNumber>2</sequenceNumber>
                  <type>2</type>
                  <referenceNumber>referenceNumber2</referenceNumber>
                </TransportDocument>
              </Consignment>
              <GoodsItem>
                <declarationGoodsItemNumber>1</declarationGoodsItemNumber>
                <referenceNumberUCR>referenceNumberUcr</referenceNumberUCR>
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
                <Packaging>
                  <sequenceNumber>2</sequenceNumber>
                  <typeOfPackages>typeOfPackages</typeOfPackages>
                  <numberOfPackages>1</numberOfPackages>
                  <shippingMarks>shippingMarks</shippingMarks>
                </Packaging>
              </GoodsItem>
              <GoodsItem>
                <declarationGoodsItemNumber>2</declarationGoodsItemNumber>
                <referenceNumberUCR>referenceNumberUcr</referenceNumberUCR>
                <Commodity>
                  <GoodsMeasure>
                    <grossMass>100.55</grossMass>
                    <netMass>80.45</netMass>
                  </GoodsMeasure>
                </Commodity>
                <Packaging>
                  <sequenceNumber>3</sequenceNumber>
                  <typeOfPackages>typeOfPackages</typeOfPackages>
                  <numberOfPackages>1</numberOfPackages>
                  <shippingMarks>shippingMarks</shippingMarks>
                </Packaging>
                <Packaging>
                  <sequenceNumber>4</sequenceNumber>
                  <typeOfPackages>typeOfPackages</typeOfPackages>
                  <numberOfPackages>1</numberOfPackages>
                  <shippingMarks>shippingMarks</shippingMarks>
                </Packaging>
              </GoodsItem>
            </GoodsShipment>
            <updatedAt>2026-08-11T00:00:00</updatedAt>
            <metadata>
              <error>
                <code>CODE_1</code>
                <description>description</description>
                <path>path</path>
                <originalValue>originalValue1</originalValue>
              </error>
              <error>
                <code>CODE_2</code>
                <description>description</description>
                <path>path</path>
                <originalValue>originalValue2</originalValue>
              </error>
            </metadata>
          </Submission>
        end xml

        XmlOps.normalize(TestData.submission.toXmlRoot) shouldBe XmlOps.normalize(xml)
      }

      "when non root optional fields are missing" in {
        val xml: Elem =
          <Submission>
            <submissionId>{TestData.id}</submissionId>
            <status>0</status>
            <ExportOperation>
              <type>1</type>
              <MRN>mrn</MRN>
              <discrepanciesExist>1</discrepanciesExist>
              <splitIndicator>1</splitIndicator>
            </ExportOperation>
            <CustomsOfficeOfExitActual>
              <referenceNumber>referenceNumber</referenceNumber>
            </CustomsOfficeOfExitActual>
            <GoodsShipment>
              <Consignment>
                <referenceNumberUCR>referenceNumberUcr</referenceNumberUCR>
                <TransportEquipment>
                  <Seal></Seal>
                  <GoodsReference></GoodsReference>
                </TransportEquipment>
                <TransportEquipment>
                  <Seal></Seal>
                  <GoodsReference></GoodsReference>
                </TransportEquipment>
                <LocationOfGoods>
                  <typeOfLocation>typeOfLocation</typeOfLocation>
                  <qualifierOfIdentification>qualifierOfIdentification</qualifierOfIdentification>
                </LocationOfGoods>
                <ActiveBorderTransportMeans></ActiveBorderTransportMeans>
                <TransportDocument></TransportDocument>
                <TransportDocument></TransportDocument>
              </Consignment>
              <GoodsItem>
                <Commodity>
                  <GoodsMeasure>
                    <grossMass>100.55</grossMass>
                    <netMass>80.45</netMass>
                  </GoodsMeasure>
                </Commodity>
                <Packaging></Packaging>
                <Packaging></Packaging>
              </GoodsItem>
              <GoodsItem>
                <Commodity>
                  <GoodsMeasure>
                    <grossMass>100.55</grossMass>
                    <netMass>80.45</netMass>
                  </GoodsMeasure>
                </Commodity>
                <Packaging></Packaging>
                <Packaging></Packaging>
              </GoodsItem>
            </GoodsShipment>
            <updatedAt>2026-08-11T00:00:00</updatedAt>
          </Submission>
        end xml

        XmlOps.normalize(TestData.submissionNoNonRootOptionals.toXmlRoot) shouldBe XmlOps.normalize(xml)
      }

      "when GoodsShipment children optional fields are missing" in {
        val xml: Elem =
          <Submission>
            <submissionId>{TestData.id}</submissionId>
            <status>1</status>
            <ExportOperation>
              <type>1</type>
              <MRN>mrn</MRN>
              <discrepanciesExist>1</discrepanciesExist>
              <splitIndicator>1</splitIndicator>
            </ExportOperation>
            <CustomsOfficeOfExitActual>
              <referenceNumber>referenceNumber</referenceNumber>
            </CustomsOfficeOfExitActual>
            <GoodsShipment>
              <Consignment>
                <referenceNumberUCR>referenceNumberUcr</referenceNumberUCR>
                <LocationOfGoods>
                  <typeOfLocation>typeOfLocation</typeOfLocation>
                  <qualifierOfIdentification>qualifierOfIdentification</qualifierOfIdentification>
                </LocationOfGoods>
              </Consignment>
            </GoodsShipment>
            <updatedAt>2026-08-11T00:00:00</updatedAt>
          </Submission>
        end xml

        XmlOps.normalize(TestData.submissionNoGoodsShipmentChildrenOptionals.toXmlRoot) shouldBe XmlOps.normalize(xml)
      }

      "when GoodsShipment is missing" in {
        val xml: Elem =
          <Submission>
            <submissionId>{TestData.id}</submissionId>
            <status>3</status>
            <ExportOperation>
              <type>1</type>
              <MRN>mrn</MRN>
              <discrepanciesExist>1</discrepanciesExist>
              <splitIndicator>1</splitIndicator>
            </ExportOperation>
            <CustomsOfficeOfExitActual>
              <referenceNumber>referenceNumber</referenceNumber>
            </CustomsOfficeOfExitActual>
            <updatedAt>2026-08-11T00:00:00</updatedAt>
          </Submission>

        XmlOps.normalize(TestData.submissionNoGoodsShipment.toXmlRoot) shouldBe XmlOps.normalize(xml)
      }
    }

    ".fromMongoAesIE507Message" - {

      "should correctly map a MongoAesIE507Message to a Submission" - {

        "when there is only one NotificationEvent" in {
          val submission: Submission =
            Submission(
              submissionId = SubmissionId(TestData.id),
              status = NotificationEventStatus.Awaiting,
              exportOperation = TestData.mongoAesIE507MessageOneEvent.exportOperation,
              customsOfficeOfExitActual = TestData.mongoAesIE507MessageOneEvent.customsOfficeOfExitActual,
              goodsShipment = TestData.mongoAesIE507MessageOneEvent.goodsShipment,
              updatedAt = TestData.dateTime,
              metadata = None
            )

          Submission.fromMongoAesIE507Message(TestData.mongoAesIE507MessageOneEvent) shouldBe submission
        }

        "when there are multiple NotificationEvent, will select the most recent" in {
          val submission: Submission =
            Submission(
              submissionId = SubmissionId(TestData.id),
              status = NotificationEventStatus.Rejected,
              exportOperation = TestData.mongoAesIE507MessageOneEvent.exportOperation,
              customsOfficeOfExitActual = TestData.mongoAesIE507MessageOneEvent.customsOfficeOfExitActual,
              goodsShipment = TestData.mongoAesIE507MessageOneEvent.goodsShipment,
              updatedAt = TestData.dateTime,
              metadata = Some(NonEmptyList.of(TestData.notificationError1, TestData.notificationError2))
            )

          Submission.fromMongoAesIE507Message(TestData.mongoAesIE507MessageMultipleEvents) shouldBe submission
        }
      }
    }
  }
