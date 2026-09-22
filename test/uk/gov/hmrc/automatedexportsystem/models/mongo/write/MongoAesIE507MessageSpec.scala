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

package uk.gov.hmrc.automatedexportsystem.models.mongo.write

import cats.data.NonEmptyList
import org.scalatest.EitherValues
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import play.api.libs.json.{JsValue, Json}
import uk.gov.hmrc.automatedexportsystem.generators.MongoAesIE507MessageGenerator
import uk.gov.hmrc.automatedexportsystem.models.IE507.*
import uk.gov.hmrc.automatedexportsystem.models.IE507.aes.SubmissionId
import uk.gov.hmrc.automatedexportsystem.models.notification.{NotificationError, NotificationEvent, NotificationEventStatus}

import java.time.Instant
import java.util.UUID

class MongoAesIE507MessageSpec extends AnyFreeSpecLike, Matchers, EitherValues, ScalaCheckDrivenPropertyChecks, MongoAesIE507MessageGenerator:
  object TestData:
    val id: UUID = UUID.fromString("6fb33641-6dc7-4a4f-adef-06238c13a317")

    val instant: Long = Instant.parse("2026-07-21T00:00:00.000Z").toEpochMilli

    val mongoAesIE507MessageAllFields: MongoAesIE507Message =
      MongoAesIE507Message(
        submissionId = SubmissionId(id),
        eoriNumber = EoriNumber("eoriNumber"),
        createdAt = Instant.ofEpochMilli(instant),
        updatedAt = Instant.ofEpochMilli(instant),
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
                    containerIdentificationNumber = Some(ContainerIdentificationNumber("some-number")),
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
                qualifierOfIdentification = QualifierOfIdentification("qualifierIdentification"),
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
                  referenceNumberUcr = None,
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
            correlationId = "correlationId",
            dateCreated = Instant.ofEpochMilli(instant),
            dateUpdated = Instant.ofEpochMilli(instant),
            isPending = false,
            status = NotificationEventStatus.Awaiting,
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
        )
      )

    val mongoAesIE507MessageAllFieldsJson: JsValue =
      Json.parse(s"""
          |{
          |  "submissionId" : "6fb33641-6dc7-4a4f-adef-06238c13a317",
          |  "eoriNumber" : "eoriNumber",
          |  "createdAt" : {
          |    "$$date" : {
          |      "$$numberLong" : "$instant"
          |    }
          |  },
          |  "updatedAt" : {
          |    "$$date" : {
          |      "$$numberLong" : "$instant"
          |    }
          |  },
          |  "exportOperation" : {
          |    "exportOperationType" : 1,
          |    "mrn" : "mrn",
          |    "discrepanciesExist" : false,
          |    "splitIndicator" : true
          |  },
          |  "customsOfficeOfExitActual" : {
          |    "referenceNumber" : "referenceNumber"
          |  },
          |  "goodsShipment" : {
          |    "consignment" : {
          |      "modeOfTransportAtTheBorder" : 1,
          |      "referenceNumberUCR" : "referenceNumberUcr",
          |      "parentUcrId" : "parentUcrId",
          |      "transportEquipment" : [ {
          |        "sequenceNumber" : 1,
          |        "containerIdentificationNumber" : "some-number",
          |        "numberOfSeals" : 1,
          |        "seal" : [ {
          |          "sequenceNumber" : 1,
          |          "sealIdentifier" : "sealIdentifier"
          |        } ],
          |        "goodsReference" : [ {
          |          "sequenceNumber" : 1,
          |          "declarationGoodsItemNumber" : 1
          |        } ]
          |      } ],
          |      "locationOfGoods" : {
          |        "typeOfLocation" : "typeOfLocation",
          |        "qualifierOfIdentification" : "qualifierIdentification",
          |        "authorisationNumber" : "authorisationNumber",
          |        "additionalIdentifier" : "additionalIdentifier",
          |        "unLocode" : "unLocode"
          |      },
          |      "activeBorderTransportMeans" : {
          |        "typeOfIdentification" : "typeOfIdentification",
          |        "identificationNumber" : "identificationNumber",
          |        "nationality" : "nationality"
          |      },
          |      "transportDocument" : [ {
          |        "sequenceNumber" : 1,
          |        "transportDocumentType" : 1,
          |        "referenceNumber" : "referenceNumber"
          |      } ]
          |    },
          |    "goodsItem" : [ {
          |      "declarationGoodsItemNumber" : 1,
          |      "commodity" : {
          |        "goodsMeasure": {
          |          "grossMass": 100.55,
          |          "netMass": 80.45
          |        }
          |      },
          |      "packaging" : [ {
          |        "sequenceNumber" : 1,
          |        "typeOfPackages" : "typeOfPackages",
          |        "numberOfPackages" : 1,
          |        "shippingMarks" : "shippingMarks"
          |      } ]
          |    } ]
          |  },
          |  "metadata" : [ {
          |    "correlationId" : "correlationId",
          |    "dateCreated" : {
          |      "$$date" : {
          |        "$$numberLong" : "$instant"
          |      }
          |    },
          |    "dateUpdated" : {
          |      "$$date" : {
          |        "$$numberLong" : "$instant"
          |      }
          |    },
          |    "isPending" : false,
          |    "status" : 0,
          |    "errors" : [ {
          |      "code" : "code",
          |      "description" : "description",
          |      "path" : "path",
          |      "originalValue": "originalValue"
          |    } ]
          |  } ]
          |}
          |""".stripMargin)

    val mongoAesIE507MessageNoOptionalGoodsShipment: MongoAesIE507Message =
      MongoAesIE507Message(
        submissionId = SubmissionId(id),
        eoriNumber = EoriNumber("eoriNumber"),
        createdAt = Instant.ofEpochMilli(instant),
        updatedAt = Instant.ofEpochMilli(instant),
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
            correlationId = "correlationId",
            dateCreated = Instant.ofEpochMilli(instant),
            dateUpdated = Instant.ofEpochMilli(instant),
            isPending = false,
            status = NotificationEventStatus.Awaiting,
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
        )
      )

    val mongoAesIE507MessageNoOptionalGoodsShipmentJson: JsValue =
      Json.parse(s"""
          |{
          |  "submissionId" : "6fb33641-6dc7-4a4f-adef-06238c13a317",
          |  "eoriNumber" : "eoriNumber",
          |  "createdAt" : {
          |    "$$date" : {
          |      "$$numberLong" : "$instant"
          |    }
          |  },
          |  "updatedAt" : {
          |    "$$date" : {
          |      "$$numberLong" : "$instant"
          |    }
          |  },
          |  "exportOperation" : {
          |    "exportOperationType" : 1,
          |    "mrn" : "mrn",
          |    "discrepanciesExist" : false,
          |    "splitIndicator" : true
          |  },
          |  "customsOfficeOfExitActual" : {
          |    "referenceNumber" : "referenceNumber"
          |  },
          |  "metadata" : [ {
          |    "correlationId" : "correlationId",
          |    "dateCreated" : {
          |      "$$date" : {
          |        "$$numberLong" : "$instant"
          |      }
          |    },
          |    "dateUpdated" : {
          |      "$$date" : {
          |        "$$numberLong" : "$instant"
          |      }
          |    },
          |    "isPending" : false,
          |    "status" : 0,
          |    "errors" : [ {
          |      "code" : "code",
          |      "description" : "description",
          |      "path" : "path",
          |      "originalValue": "originalValue"
          |    } ]
          |  } ]
          |}
          |""".stripMargin)

    val mongoAesIE507MessageNoObjOptionals: MongoAesIE507Message =
      MongoAesIE507Message(
        submissionId = SubmissionId(id),
        eoriNumber = EoriNumber("eoriNumber"),
        createdAt = Instant.ofEpochMilli(instant),
        updatedAt = Instant.ofEpochMilli(instant),
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
              modeOfTransportAtTheBorder = None,
              referenceNumberUCR = ReferenceNumberUcr("referenceNumberUcr"),
              parentUcrId = None,
              transportEquipment = Some(
                NonEmptyList.one(
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
                qualifierOfIdentification = QualifierOfIdentification("qualifierIdentification"),
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
                NonEmptyList.one(
                  TransportDocument(
                    sequenceNumber = None,
                    transportDocumentType = None,
                    referenceNumber = None
                  )
                )
              )
            ),
            goodsItem = Some(
              NonEmptyList.one(
                GoodsItem(
                  referenceNumberUcr = None,
                  declarationGoodsItemNumber = None,
                  commodity = Commodity(
                    goodsMeasure = GoodsMeasure(
                      grossMass = GrossMass(100.55),
                      netMass = NetMass(80.45)
                    )
                  ),
                  packaging = Some(
                    NonEmptyList.one(
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
        metadata = NonEmptyList.one(
          NotificationEvent(
            correlationId = "correlationId",
            dateCreated = Instant.ofEpochMilli(instant),
            dateUpdated = Instant.ofEpochMilli(instant),
            isPending = false,
            status = NotificationEventStatus.Awaiting,
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
        )
      )

    val mongoAesIE507MessageNoObjOptionalsJson: JsValue =
      Json.parse(s"""
          |{
          |  "submissionId" : "6fb33641-6dc7-4a4f-adef-06238c13a317",
          |  "eoriNumber" : "eoriNumber",
          |  "createdAt" : {
          |    "$$date" : {
          |      "$$numberLong" : "$instant"
          |    }
          |  },
          |  "updatedAt" : {
          |    "$$date" : {
          |      "$$numberLong" : "$instant"
          |    }
          |  },
          |  "exportOperation" : {
          |    "exportOperationType" : 1,
          |    "mrn" : "mrn",
          |    "discrepanciesExist" : false,
          |    "splitIndicator" : true
          |  },
          |  "customsOfficeOfExitActual" : {
          |    "referenceNumber" : "referenceNumber"
          |  },
          |  "goodsShipment" : {
          |    "consignment" : {
          |      "referenceNumberUCR" : "referenceNumberUcr",
          |      "transportEquipment" : [ {
          |        "seal" : [ { } ],
          |        "goodsReference" : [ { } ]
          |      } ],
          |      "locationOfGoods" : {
          |        "typeOfLocation" : "typeOfLocation",
          |        "qualifierOfIdentification" : "qualifierIdentification"
          |      },
          |      "activeBorderTransportMeans" : { },
          |      "transportDocument" : [ { } ]
          |    },
          |    "goodsItem" : [ {
          |      "commodity" : {
          |        "goodsMeasure": {
          |          "grossMass": 100.55,
          |          "netMass": 80.45
          |        }
          |      },
          |      "packaging" : [ { } ]
          |    } ]
          |  },
          |  "metadata" : [ {
          |    "correlationId" : "correlationId",
          |    "dateCreated" : {
          |      "$$date" : {
          |        "$$numberLong" : "$instant"
          |      }
          |    },
          |    "dateUpdated" : {
          |      "$$date" : {
          |        "$$numberLong" : "$instant"
          |      }
          |    },
          |    "isPending" : false,
          |    "status" : 0,
          |    "errors" : [ {
          |      "code" : "code",
          |      "description" : "description",
          |      "path" : "path",
          |      "originalValue": "originalValue"
          |    } ]
          |  } ]
          |}
          |""".stripMargin)

    val mongoAesIE507MessageEmptyLists: MongoAesIE507Message =
      MongoAesIE507Message(
        submissionId = SubmissionId(id),
        eoriNumber = EoriNumber("eoriNumber"),
        createdAt = Instant.ofEpochMilli(instant),
        updatedAt = Instant.ofEpochMilli(instant),
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
              transportEquipment = None,
              locationOfGoods = LocationOfGoods(
                typeOfLocation = TypeOfLocation("typeOfLocation"),
                qualifierOfIdentification = QualifierOfIdentification("qualifierIdentification"),
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
              transportDocument = None
            ),
            goodsItem = None
          )
        ),
        metadata = NonEmptyList.one(
          NotificationEvent(
            correlationId = "correlationId",
            dateCreated = Instant.ofEpochMilli(instant),
            dateUpdated = Instant.ofEpochMilli(instant),
            isPending = false,
            status = NotificationEventStatus.Awaiting,
            errors = None
          )
        )
      )

    val mongoAesIE507MessageEmptyListsJson: JsValue =
      Json.parse(s"""
          |{
          |  "submissionId" : "6fb33641-6dc7-4a4f-adef-06238c13a317",
          |  "eoriNumber" : "eoriNumber",
          |  "createdAt" : {
          |    "$$date" : {
          |      "$$numberLong" : "$instant"
          |    }
          |  },
          |  "updatedAt" : {
          |    "$$date" : {
          |      "$$numberLong" : "$instant"
          |    }
          |  },
          |  "exportOperation" : {
          |    "exportOperationType" : 1,
          |    "mrn" : "mrn",
          |    "discrepanciesExist" : false,
          |    "splitIndicator" : true
          |  },
          |  "customsOfficeOfExitActual" : {
          |    "referenceNumber" : "referenceNumber"
          |  },
          |  "goodsShipment" : {
          |    "consignment" : {
          |      "modeOfTransportAtTheBorder" : 1,
          |      "referenceNumberUCR" : "referenceNumberUcr",
          |      "parentUcrId" : "parentUcrId",
          |      "locationOfGoods" : {
          |        "typeOfLocation" : "typeOfLocation",
          |        "qualifierOfIdentification" : "qualifierIdentification",
          |        "authorisationNumber" : "authorisationNumber",
          |        "additionalIdentifier" : "additionalIdentifier",
          |        "unLocode" : "unLocode"
          |      },
          |      "activeBorderTransportMeans" : {
          |        "typeOfIdentification" : "typeOfIdentification",
          |        "identificationNumber" : "identificationNumber",
          |        "nationality" : "nationality"
          |      }
          |    }
          |  },
          |  "metadata" : [ {
          |    "correlationId" : "correlationId",
          |    "dateCreated" : {
          |      "$$date" : {
          |        "$$numberLong" : "$instant"
          |      }
          |    },
          |    "dateUpdated" : {
          |      "$$date" : {
          |        "$$numberLong" : "$instant"
          |      }
          |    },
          |    "isPending" : false,
          |    "status" : 0
          |  } ]
          |}
          |""".stripMargin)

  end TestData

  "MongoAesIE507Message" - {

    ".mongoFormat" - {

      "when all optional fields are present" - {

        "should read a JsValue" - {

          "successfully" in {
            MongoAesIE507Message.mongoFormat
              .reads(TestData.mongoAesIE507MessageAllFieldsJson)
              .asEither
              .value shouldBe TestData.mongoAesIE507MessageAllFields
          }
        }

        "should write a JsValue" - {

          "successfully" in {
            MongoAesIE507Message.mongoFormat
              .writes(TestData.mongoAesIE507MessageAllFields) shouldBe
              TestData.mongoAesIE507MessageAllFieldsJson
          }
        }
      }

      "when all optional fields are missing" - {

        "should read a JsValue" - {

          "successfully" in {
            MongoAesIE507Message.mongoFormat
              .reads(TestData.mongoAesIE507MessageNoOptionalGoodsShipmentJson)
              .asEither
              .value shouldBe TestData.mongoAesIE507MessageNoOptionalGoodsShipment
          }
        }

        "should write a JsValue" - {

          "successfully" in {
            MongoAesIE507Message.mongoFormat
              .writes(TestData.mongoAesIE507MessageNoOptionalGoodsShipment) shouldBe
              TestData.mongoAesIE507MessageNoOptionalGoodsShipmentJson
          }
        }
      }

      "when some optional fields are missing" - {

        "should read a JsValue" - {

          "successfully" in {
            MongoAesIE507Message.mongoFormat
              .reads(TestData.mongoAesIE507MessageNoObjOptionalsJson)
              .asEither
              .value shouldBe TestData.mongoAesIE507MessageNoObjOptionals
          }
        }

        "should write a JsValue" - {

          "successfully" in {
            MongoAesIE507Message.mongoFormat
              .writes(TestData.mongoAesIE507MessageNoObjOptionals) shouldBe
              TestData.mongoAesIE507MessageNoObjOptionalsJson
          }
        }
      }

      "when list fields are missing" - {

        "should read a JsValue" - {

          "successfully" in {
            MongoAesIE507Message.mongoFormat
              .reads(TestData.mongoAesIE507MessageEmptyListsJson)
              .asEither
              .value shouldBe TestData.mongoAesIE507MessageEmptyLists
          }
        }

        "should write a JsValue" - {

          "successfully" in {
            MongoAesIE507Message.mongoFormat
              .writes(TestData.mongoAesIE507MessageNoObjOptionals) shouldBe
              TestData.mongoAesIE507MessageNoObjOptionalsJson
          }
        }
      }

      "should read and write" - {

        "successfully" in forAll((mongoAesIE507Message: MongoAesIE507Message) =>
          val json: JsValue = MongoAesIE507Message.mongoFormat.writes(mongoAesIE507Message)

          MongoAesIE507Message.mongoFormat.reads(json).asEither.value shouldBe mongoAesIE507Message
        )
      }
    }
  }
