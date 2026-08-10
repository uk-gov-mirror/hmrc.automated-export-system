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

package uk.gov.hmrc.automatedexportsystem.xml

import cats.data.{NonEmptyList, Validated}
import cats.implicits.catsSyntaxTuple4Semigroupal
import cats.syntax.all.catsSyntaxTuple3Semigroupal
import org.scalatest.EitherValues
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor1, TableFor2}
import uk.gov.hmrc.automatedexportsystem.errors.XmlReaderError
import uk.gov.hmrc.automatedexportsystem.xml.XmlReader.{as, nonEmptyReader}

import scala.xml.{Elem, NodeSeq}
class XmlReaderSpec extends AnyFreeSpecLike, Matchers, EitherValues, TableDrivenPropertyChecks:
  object TestData:
    val unitUnsuccessfulReader: XmlReader[Unit] = XmlReader.nonEmptyReader { (xml, path) =>
      Validated.invalidNel(XmlReaderError.ParseError(path.toString, s"Failed to parse '${xml.text.trim}' to Unit"))
    }

    val unitSuccessfulReader: XmlReader[Unit] = XmlReader.nonEmptyReader { (_, _) =>
      Validated.validNel(())
    }

    case class SimpleModel(a: Int, b: String, c: Boolean)

    object SimpleModel:
      given simpleModelXmlReader: XmlReader[SimpleModel] = nonEmptyReader { (xml, path) =>
        (
          (XmlPath \ "a").read[Int](xml, path),
          (XmlPath \ "b").read[String](xml, path),
          (XmlPath \ "c").read[Boolean](xml, path)
        ).mapN(SimpleModel.apply)
      }

    case class ComplexModel(a: SimpleModel, b: Option[SimpleModel], c: List[SimpleModel], d: NonEmptyList[SimpleModel])

    object ComplexModel:
      given complexModelXmlReader: XmlReader[ComplexModel] = nonEmptyReader { (xml, path) =>
        (
          (XmlPath \ "a").read[SimpleModel](xml, path),
          (XmlPath \ "b").read[Option[SimpleModel]](xml, path),
          (XmlPath \ "c").read[List[SimpleModel]](xml, path),
          (XmlPath \ "d").read[NonEmptyList[SimpleModel]](xml, path)
        ).mapN(ComplexModel.apply)
      }

  "XmlReader" - {

    "should read primitive elements" - {

      "Int" - {
        val intReader: XmlReader[Int] = implicitly

        "successfully" in {
          val intTable: TableFor2[Elem, Int] = Table(
            ("xml", "int"),
            (<xml>123</xml>, 123),
            (<xml>48123817</xml>, 48123817),
            (<xml> 0 </xml>, 0),
            (<xml>  {Int.MaxValue}</xml>, Int.MaxValue),
            (<xml>{Int.MinValue}  </xml>, Int.MinValue)
          )

          forAll(intTable) { (xml, int) =>
            intReader.read(xml, XmlPath).toEither.value shouldBe int
          }
        }

        "unsuccessfully" in {
          val intTable: TableFor2[Elem, XmlReaderError] = Table(
            ("xml", "error"),
            (<xml></xml>, XmlReaderError.MissingOrEmpty("/")),
            (<xml>   </xml>, XmlReaderError.MissingOrEmpty("/")),
            (<xml> 1239ASD  </xml>, XmlReaderError.ParseError("/", s"Failed to parse '1239ASD' to Int"))
          )

          forAll(intTable) { (xml, error) =>
            intReader.read(xml, XmlPath).toEither.left.value shouldBe NonEmptyList.one(error)
          }
        }
      }

      "String" - {

        "successfully" in {
          val stringReader: XmlReader[String] = implicitly

          val stringTable: TableFor2[Elem, String] = Table(
            ("xml", "string"),
            (<xml>abc</xml>, "abc"),
            (<xml> abcdefghijklmnopqrstuvwxyz  </xml>, "abcdefghijklmnopqrstuvwxyz"),
            (<xml>123  </xml>, "123")
          )

          forAll(stringTable) { (xml, string) =>
            stringReader.read(xml, XmlPath).toEither.value shouldBe string
          }
        }

        "unsuccessfully" in {
          val stringReader: XmlReader[String] = implicitly

          val stringTable: TableFor2[Elem, XmlReaderError] = Table(
            ("xml", "error"),
            (<xml></xml>, XmlReaderError.MissingOrEmpty("/")),
            (<xml>    </xml>, XmlReaderError.MissingOrEmpty("/"))
          )

          forAll(stringTable) { (xml, error) =>
            stringReader.read(xml, XmlPath).toEither.left.value shouldBe NonEmptyList.one(error)
          }
        }
      }

      "Boolean" - {
        val booleanReader: XmlReader[Boolean] = implicitly

        "successfully" in {
          val intTable: TableFor2[Elem, Boolean] = Table(
            ("xml", "bool"),
            (<xml>true</xml>, true),
            (<xml>false</xml>, false),
            (<xml> 1</xml>, true),
            (<xml>0 </xml>, false)
          )

          forAll(intTable) { (xml, bool) =>
            booleanReader.read(xml, XmlPath).toEither.value shouldBe bool
          }
        }

        "unsuccessfully" in {
          val booleanTable: TableFor2[Elem, XmlReaderError] = Table(
            ("xml", "error"),
            (<xml></xml>, XmlReaderError.MissingOrEmpty("/")),
            (<xml></xml>, XmlReaderError.MissingOrEmpty("/")),
            (<xml>not true </xml>, XmlReaderError.ParseError("/", s"Failed to parse 'not true' to Boolean")),
            (<xml> not false</xml>, XmlReaderError.ParseError("/", s"Failed to parse 'not false' to Boolean"))
          )

          forAll(booleanTable) { (xml, error) =>
            booleanReader.read(xml, XmlPath).toEither.left.value shouldBe NonEmptyList.one(error)
          }
        }
      }
    }

    ".nonEmptyReader" - {

      "should return an XmlReader that reads elements" - {
        val nonEmptyReader: XmlReader[Unit] = XmlReader.nonEmptyReader { (_, _) =>
          Validated.validNel(())
        }

        "successfully" - {

          "when not missing or empty" in {
            val testTable: TableFor1[Elem] = Table(
              "xml",
              <xml>not missing</xml>,
              <xml>  not   empty   </xml>,
              <xml>  not  missing or   empty   </xml>
            )

            forAll(testTable)(xml => nonEmptyReader.read(xml, XmlPath).toEither.value shouldBe ())
          }
        }

        "unsuccessfully" - {

          "when missing" in {
            nonEmptyReader.read(NodeSeq.Empty, XmlPath).toEither.left.value shouldBe
              NonEmptyList.one(XmlReaderError.MissingOrEmpty("/"))
          }

          "when empty" in {
            val testTable: TableFor1[Elem] = Table(
              "xml",
              <xml></xml>,
              <xml>    </xml>
            )

            forAll(testTable)(xml =>
              nonEmptyReader.read(xml, XmlPath).toEither.left.value shouldBe
                NonEmptyList.one(XmlReaderError.MissingOrEmpty("/"))
            )
          }
        }
      }
    }

    ".optionReader" - {

      "should return an XmlReader that reads elements" - {

        "successfully" - {
          val optionReader: XmlReader[Option[Unit]] = XmlReader.optionReader(using TestData.unitSuccessfulReader)

          "when not missing or empty" in {
            val testTable: TableFor1[Elem] = Table(
              "xml",
              <xml>not missing</xml>,
              <xml>  not   empty   </xml>,
              <xml>  not  missing or   empty   </xml>
            )

            forAll(testTable)(xml => optionReader.read(xml, XmlPath).toEither.value shouldBe Some(()))
          }

          "when missing" in {
            optionReader.read(NodeSeq.Empty, XmlPath).toEither.value shouldBe None
          }

          "when empty" in {
            val testTable: TableFor1[Elem] = Table(
              "xml",
              <xml></xml>,
              <xml>    </xml>
            )

            forAll(testTable)(xml => optionReader.read(xml, XmlPath).toEither.value shouldBe None)
          }
        }

        "unsuccessfully" - {
          val optionReader: XmlReader[Option[Unit]] = XmlReader.optionReader(using TestData.unitUnsuccessfulReader)

          "when not missing or empty" - {

            "and the underlying XmlReader reads unsuccessfully" in {
              val xml: Elem =
                <xml>unit</xml>

              val error: XmlReaderError =
                XmlReaderError.ParseError("/", s"Failed to parse 'unit' to Unit")

              optionReader.read(xml, XmlPath).toEither.left.value shouldBe NonEmptyList.one(error)
            }
          }
        }
      }
    }

    ".listReader" - {

      "should return an XmlReader that reads elements" - {
        val listIntReader: XmlReader[List[Int]] = implicitly

        "successfully" - {

          "when there are no elements" in {
            listIntReader.read(NodeSeq.Empty, XmlPath).toEither.value shouldBe Nil
          }

          "when there are multiple elements" in {
            val xml: NodeSeq =
              <xml1>1</xml1>
                <xml2>2</xml2>
                <xml3>3</xml3>

            listIntReader.read(xml, XmlPath).toEither.value shouldBe List(1, 2, 3)
          }
        }

        "unsuccessfully" - {

          "when there is at least one element" - {

            "and the underlying XmlReader reads unsuccessfully" - {

              "for one element" in {
                val xml: NodeSeq =
                  <xml1>1</xml1>
                    <xml2>2</xml2>
                    <xml3>three</xml3>

                listIntReader.read(xml, XmlPath).toEither.left.value shouldBe NonEmptyList.one(
                  XmlReaderError.ParseError(s"/[2]", "Failed to parse 'three' to Int")
                )
              }

              "for multiple elements" in {
                val xml: NodeSeq =
                  <xml1>1</xml1>
                    <xml2></xml2>
                    <xml3>3</xml3>
                    <xml4>four</xml4>
                    <xml5></xml5>
                    <xml6>six</xml6>

                val errors: NonEmptyList[XmlReaderError] =
                  NonEmptyList.of(
                    XmlReaderError.MissingOrEmpty("/[1]"),
                    XmlReaderError.ParseError("/[3]", "Failed to parse 'four' to Int"),
                    XmlReaderError.MissingOrEmpty("/[4]"),
                    XmlReaderError.ParseError("/[5]", "Failed to parse 'six' to Int")
                  )

                listIntReader.read(xml, XmlPath).toEither.left.value shouldBe errors
              }
            }
          }
        }
      }
    }

    ".nonEmptyListReader" - {

      "should return an XmlReader that reads elements" - {
        val nelIntReader: XmlReader[NonEmptyList[Int]] = implicitly

        "successfully" - {

          "when there is at least one element" - {

            "one element" in {
              val xml: NodeSeq =
                <xml>1</xml>

              nelIntReader.read(xml, XmlPath).toEither.value shouldBe NonEmptyList.one(1)
            }

            "multiple elements" in {
              val xml: NodeSeq =
                <xml1>1</xml1>
                  <xml2>2</xml2>
                  <xml3>3</xml3>
                  <xml4>4</xml4>
                  <xml5>5</xml5>
                  <xml6>6</xml6>

              nelIntReader.read(xml, XmlPath).toEither.value shouldBe NonEmptyList.of(1, 2, 3, 4, 5, 6)
            }
          }
        }

        "unsuccessfully" - {

          "when there are no elements" in {
            nelIntReader.read(NodeSeq.Empty, XmlPath).toEither.left.value shouldBe NonEmptyList.one(
              XmlReaderError.ParseError("/", "Failed to parse empty list into NonEmptyList")
            )
          }

          "when there is at least one element" - {

            "and the underlying XmlReader reads unsuccessfully" - {

              "for one element" in {
                val xml: NodeSeq =
                  <xml1>1</xml1>
                    <xml2>2</xml2>
                    <xml3>three</xml3>

                nelIntReader.read(xml, XmlPath).toEither.left.value shouldBe NonEmptyList.one(
                  XmlReaderError.ParseError("/[2]", "Failed to parse 'three' to Int")
                )
              }

              "for multiple elements" in {
                val xml: NodeSeq =
                  <xml1>1</xml1>
                    <xml2></xml2>
                    <xml3>3</xml3>
                    <xml4>four</xml4>
                    <xml5></xml5>
                    <xml6>six</xml6>

                val errors: NonEmptyList[XmlReaderError] =
                  NonEmptyList.of(
                    XmlReaderError.MissingOrEmpty("/[1]"),
                    XmlReaderError.ParseError("/[3]", "Failed to parse 'four' to Int"),
                    XmlReaderError.MissingOrEmpty("/[4]"),
                    XmlReaderError.ParseError("/[5]", "Failed to parse 'six' to Int")
                  )

                nelIntReader.read(xml, XmlPath).toEither.left.value shouldBe errors
              }
            }
          }
        }
      }
    }

    ".as" - {

      "should read a simple NodeSeq" - {

        "when there is an implicit XmlReader instance available" - {

          "successfully" - {

            "when there are no errors encountered" in {
              val xml1: Elem =
                <xml>
                  <a>1</a>
                  <b>string1</b>
                  <c>true</c>
                </xml>

              val xml2: Elem =
                <xml>
                  <a>2</a>
                  <b>string2</b>
                  <c>1</c>
                </xml>

              val xml3: Elem =
                <xml>
                  <a>3</a>
                  <b>string3</b>
                  <c>false</c>
                </xml>

              val xml4: Elem =
                <xml>
                  <a>4</a>
                  <b>string4</b>
                  <c>0</c>
                </xml>

              val simpleModelTableRows: List[(Elem, TestData.SimpleModel)] =
                List(
                  (xml1, TestData.SimpleModel(1, "string1", true)),
                  (xml2, TestData.SimpleModel(2, "string2", true)),
                  (xml3, TestData.SimpleModel(3, "string3", false)),
                  (xml4, TestData.SimpleModel(4, "string4", false))
                )

              val simpleModelTable: TableFor2[Elem, TestData.SimpleModel] =
                Table(
                  ("xml", "simpleModel"),
                  simpleModelTableRows*
                )

              forAll(simpleModelTable) { (xml, simpleModel) =>
                xml.as[TestData.SimpleModel].toEither.value shouldBe simpleModel
              }
            }
          }

          "unsuccessfully" - {

            "when there is at least one error encountered" in {
              val xml1AndErrors: (Elem, NonEmptyList[XmlReaderError]) =
                (
                  <xml>
                    <a>12e</a>
                    <b></b>
                    <c>not true</c>
                  </xml>,
                  NonEmptyList.of(
                    XmlReaderError.ParseError("/a", "Failed to parse '12e' to Int"),
                    XmlReaderError.MissingOrEmpty("/b"),
                    XmlReaderError.ParseError("/c", "Failed to parse 'not true' to Boolean")
                  )
                )

              val xml2AndErrors: (Elem, NonEmptyList[XmlReaderError]) =
                (
                  <xml>
                    <a>e21</a>
                    <b>string2</b>
                    <c>not false</c>
                  </xml>,
                  NonEmptyList.of(
                    XmlReaderError.ParseError("/a", "Failed to parse 'e21' to Int"),
                    XmlReaderError.ParseError("/c", "Failed to parse 'not false' to Boolean")
                  )
                )

              val xml3AndErrors: (Elem, NonEmptyList[XmlReaderError]) =
                (
                  <xml>
                    <a>421</a>
                    <b>string3</b>
                    <c></c>
                  </xml>,
                  NonEmptyList.one(
                    XmlReaderError.MissingOrEmpty("/c")
                  )
                )

              val simpleModelTableRows: List[(Elem, NonEmptyList[XmlReaderError])] =
                List(xml1AndErrors, xml2AndErrors, xml3AndErrors)

              val simpleModelTable: TableFor2[Elem, NonEmptyList[XmlReaderError]] =
                Table(
                  ("xml", "errors"),
                  simpleModelTableRows*
                )

              forAll(simpleModelTable) { (xml, errors) =>
                xml.as[TestData.SimpleModel].toEither.left.value shouldBe errors
              }
            }
          }
        }
      }

      "should read a complex NodeSeq" - {

        "successfully" - {

          "when there are no errors encountered" in {
            val xml1: Elem =
              <xml>
                <a>
                  <a>1</a>
                  <b>string1</b>
                  <c>true</c>
                </a>
                <b>
                  <a>2</a>
                  <b>string2</b>
                  <c>1</c>
                </b>
                <c>
                  <a>31</a>
                  <b>string31</b>
                  <c>false</c>
                </c>
                <c>
                  <a>32</a>
                  <b>string32</b>
                  <c>false</c>
                </c>
                <d>
                  <a>41</a>
                  <b>string41</b>
                  <c>0</c>
                </d>
                <d>
                  <a>42</a>
                  <b>string42</b>
                  <c>0</c>
                </d>
              </xml>

            val xml2: Elem =
              <xml>
                <a>
                  <a>1</a>
                  <b>string1</b>
                  <c>true</c>
                </a>
                <c>
                  <a>31</a>
                  <b>string31</b>
                  <c>false</c>
                </c>
                <c>
                  <a>32</a>
                  <b>string32</b>
                  <c>false</c>
                </c>
                <d>
                  <a>41</a>
                  <b>string41</b>
                  <c>0</c>
                </d>
                <d>
                  <a>42</a>
                  <b>string42</b>
                  <c>0</c>
                </d>
              </xml>

            val xml3: Elem =
              <xml>
                <a>
                  <a>1</a>
                  <b>string1</b>
                  <c>true</c>
                </a>
                <d>
                  <a>41</a>
                  <b>string41</b>
                  <c>0</c>
                </d>
                <d>
                  <a>42</a>
                  <b>string42</b>
                  <c>0</c>
                </d>
              </xml>

            val xml4: Elem =
              <xml>
                <a>
                  <a>1</a>
                  <b>string1</b>
                  <c>true</c>
                </a>
                <d>
                  <a>41</a>
                  <b>string41</b>
                  <c>0</c>
                </d>
              </xml>

            val complexModelTableRows: List[(Elem, TestData.ComplexModel)] =
              List(
                (
                  xml1,
                  TestData.ComplexModel(
                    a = TestData.SimpleModel(1, "string1", true),
                    b = Some(TestData.SimpleModel(2, "string2", true)),
                    c = List(
                      TestData.SimpleModel(31, "string31", false),
                      TestData.SimpleModel(32, "string32", false)
                    ),
                    d = NonEmptyList.of(
                      TestData.SimpleModel(41, "string41", false),
                      TestData.SimpleModel(42, "string42", false)
                    )
                  )
                ),
                (
                  xml2,
                  TestData.ComplexModel(
                    a = TestData.SimpleModel(1, "string1", true),
                    b = None,
                    c = List(
                      TestData.SimpleModel(31, "string31", false),
                      TestData.SimpleModel(32, "string32", false)
                    ),
                    d = NonEmptyList.of(
                      TestData.SimpleModel(41, "string41", false),
                      TestData.SimpleModel(42, "string42", false)
                    )
                  )
                ),
                (
                  xml3,
                  TestData.ComplexModel(
                    a = TestData.SimpleModel(1, "string1", true),
                    b = None,
                    c = List.empty,
                    d = NonEmptyList.of(
                      TestData.SimpleModel(41, "string41", false),
                      TestData.SimpleModel(42, "string42", false)
                    )
                  )
                ),
                (
                  xml4,
                  TestData.ComplexModel(
                    a = TestData.SimpleModel(1, "string1", true),
                    b = None,
                    c = List.empty,
                    d = NonEmptyList.of(TestData.SimpleModel(41, "string41", false))
                  )
                )
              )

            val complexModelTable: TableFor2[Elem, TestData.ComplexModel] =
              Table(
                ("xml", "complexModel"),
                complexModelTableRows*
              )

            forAll(complexModelTable) { (xml, complexModel) =>
              xml.as[TestData.ComplexModel].toEither.value shouldBe complexModel
            }
          }

          "when there is at least one error encountered" in {
            val xml1: Elem =
              <xml>
                <b>
                  <b>string2</b>
                </b>
                <c>
                  <a>31</a>
                  <b>string31</b>
                  <c>false</c>
                </c>
                <c>
                  <a>thirty-one</a>
                  <b></b>
                  <c>not false</c>
                </c>
              </xml>

            val errors1: NonEmptyList[XmlReaderError] =
              NonEmptyList.of(
                XmlReaderError.MissingOrEmpty("/a"),
                XmlReaderError.MissingOrEmpty("/b/a"),
                XmlReaderError.MissingOrEmpty("/b/c"),
                XmlReaderError.ParseError("/c/[1]/a", "Failed to parse 'thirty-one' to Int"),
                XmlReaderError.MissingOrEmpty("/c/[1]/b"),
                XmlReaderError.ParseError("/c/[1]/c", "Failed to parse 'not false' to Boolean"),
                XmlReaderError.ParseError("/d", "Failed to parse empty list into NonEmptyList")
              )

            val xml2: Elem =
              <xml>
                <a>
                  <b>string1</b>
                  <c>not true</c>
                </a>
                <c>
                </c>
                <c>
                  <a>thirty-two</a>
                </c>
                <d>
                </d>
                <d>
                  <a>42</a>
                  <b>string42</b>
                  <c>-1</c>
                </d>
              </xml>

            val errors2: NonEmptyList[XmlReaderError] =
              NonEmptyList.of(
                XmlReaderError.MissingOrEmpty("/a/a"),
                XmlReaderError.ParseError("/a/c", "Failed to parse 'not true' to Boolean"),
                XmlReaderError.MissingOrEmpty("/c/[0]"),
                XmlReaderError.ParseError("/c/[1]/a", "Failed to parse 'thirty-two' to Int"),
                XmlReaderError.MissingOrEmpty("/c/[1]/b"),
                XmlReaderError.MissingOrEmpty("/c/[1]/c"),
                XmlReaderError.MissingOrEmpty("/d/[0]"),
                XmlReaderError.ParseError("/d/[1]/c", "Failed to parse '-1' to Boolean")
              )

            val complexModelTableRows: List[(Elem, NonEmptyList[XmlReaderError])] =
              List((xml1, errors1), (xml2, errors2))

            val complexModelTable: TableFor2[Elem, NonEmptyList[XmlReaderError]] =
              Table(
                ("xml", "errors"),
                complexModelTableRows*
              )

            forAll(complexModelTable) { (xml, errors) =>
              xml.as[TestData.ComplexModel].toEither.left.value shouldBe errors
            }
          }
        }
      }
    }
  }
