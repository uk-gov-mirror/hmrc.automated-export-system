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

import cats.data.NonEmptyList
import helpers.XmlOps
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}
import uk.gov.hmrc.automatedexportsystem.xml.XmlWriter.{toXml, toXmlRoot}

import scala.xml.{Elem, NodeSeq, Text}

class XmlWriterSpec extends AnyFreeSpecLike, Matchers, TableDrivenPropertyChecks:
  object TestData:
    val unitWriter: XmlWriter[Unit] =
      (o, label) => XmlWriter.elem(label, Text(o.toString))

    case class SimpleModel(a: Int, b: String, c: Boolean)

    object SimpleModel:
      given simpleModelXmlWriter: XmlWriter[SimpleModel] =
        (o, label) =>
          val children: NodeSeq =
            o.a.toXml("a") ++ o.b.toXml("b") ++ o.c.toXml("c")

          XmlWriter.elem(label, children)

    case class ComplexModel(a: SimpleModel, b: Option[SimpleModel], c: List[SimpleModel], d: NonEmptyList[SimpleModel])

    object ComplexModel:
      given complexModelXmlWriter: RootedXmlWriter[ComplexModel] =
        new RootedXmlWriter:
          def defaultLabel: String = "ComplexModel"

          def write(o: TestData.ComplexModel, label: String): NodeSeq =
            val children: NodeSeq =
              o.a.toXml("a")
                ++ o.b.toXml("b")
                ++ o.c.toXml("c")
                ++ o.d.toXml("d")

            XmlWriter.elem(label, children)

  "XmlWriter" - {

    "should write primitive elements" - {

      "Int" in {
        val intWriter: XmlWriter[Int] = implicitly

        val intTable: TableFor2[Int, Elem] = Table(
          ("int", "xml"),
          (123, <xml>123</xml>),
          (48123817, <xml>48123817</xml>),
          (0, <xml>0</xml>),
          (2147483647, <xml>2147483647</xml>),
          (-2147483648, <xml>-2147483648</xml>)
        )

        forAll(intTable) { (int, xml) =>
          intWriter.write(int, "xml") shouldBe xml
        }
      }

      "String" in {
        val stringWriter: XmlWriter[String] = implicitly

        val stringTable: TableFor2[String, NodeSeq] = Table(
          ("string", "xml"),
          ("abc", <xml>abc</xml>),
          ("  abcdefghijklmnopqrstuvwxyz  ", <xml>abcdefghijklmnopqrstuvwxyz</xml>),
          ("   123", <xml>123</xml>),
          ("    ", <xml></xml>)
        )

        forAll(stringTable) { (string, xml) =>
          stringWriter.write(string, "xml") shouldBe xml
        }
      }

      "Boolean" in {
        val booleanWriter: XmlWriter[Boolean] = implicitly

        val booleanTable: TableFor2[Boolean, NodeSeq] = Table(
          ("boolean", "xml"),
          (true, <xml>true</xml>),
          (false, <xml>false</xml>)
        )

        forAll(booleanTable) { (boolean, xml) =>
          booleanWriter.write(boolean, "xml") shouldBe xml
        }
      }
    }

    ".optionWriter" - {
      val optionWriter: XmlWriter[Option[Unit]] = XmlWriter.optionWriter(using TestData.unitWriter)

      "should return an XmlReader that writes optional values" in {
        val testTable: TableFor2[Option[Unit], NodeSeq] =
          Table(
            ("unit", "xml"),
            (Some(()), <xml>()</xml>),
            (None, NodeSeq.Empty)
          )

        forAll(testTable) { (unit, xml) =>
          optionWriter.write(unit, "xml") shouldBe xml
        }
      }
    }

    ".listWriter" - {
      val listStringWriter: XmlWriter[List[String]] = implicitly

      "should return an XmlWriter that writes lists of values" - {

        "when the list is empty" in {
          listStringWriter.write(List.empty, "xml") shouldBe NodeSeq.Empty
        }

        "when the list is not empty" - {

          "and the values are the empty string after trimming" in {
            val list: List[String] = List("", " ", "   ")

            val xml: NodeSeq =
              <xml></xml>
              <xml></xml>
              <xml></xml>

            listStringWriter.write(list, "xml") shouldBe xml
          }

          "and there are multiple values, empty and non-empty" in {
            val list: List[String] = List("abc", "", "  abcdefghijklmnopqrstuvwxyz  ", "  ", "  123")

            val xml: NodeSeq =
              <xml>abc</xml>
              <xml></xml>
              <xml>abcdefghijklmnopqrstuvwxyz</xml>
              <xml></xml>
              <xml>123</xml>

            listStringWriter.write(list, "xml") shouldBe xml
          }
        }
      }
    }

    ".toXml" - {

      "should write a simple ADT" - {

        "when there is an implicit XmlWriter instance available" in {
          val simpleModel1: TestData.SimpleModel = TestData.SimpleModel(1, "string1", true)

          val xml1: NodeSeq =
            <xml>
              <a>1</a>
              <b>string1</b>
              <c>true</c>
            </xml>

          val simpleModel2: TestData.SimpleModel = TestData.SimpleModel(2, "string2", false)

          val xml2: NodeSeq =
            <xml>
              <a>2</a>
              <b>string2</b>
              <c>false</c>
            </xml>

          val simpleModel3: TestData.SimpleModel = TestData.SimpleModel(3, "", true)

          val xml3: NodeSeq =
            <xml>
              <a>3</a>
              <b></b>
              <c>true</c>
            </xml>

          val simpleModelTable: TableFor2[TestData.SimpleModel, NodeSeq] =
            Table(
              ("simpleModel", "xml"),
              (simpleModel1, xml1),
              (simpleModel2, xml2),
              (simpleModel3, xml3)
            )

          forAll(simpleModelTable) { (simpleModel, xml) =>
            XmlOps.normalize(simpleModel.toXml("xml")) shouldBe XmlOps.normalize(xml)
          }
        }
      }

      "should write a complex ADT" - {

        "when there is an XmlWriter instance available" in {
          val complexModel1: TestData.ComplexModel =
            TestData.ComplexModel(
              TestData.SimpleModel(1, "string1", true),
              Some(TestData.SimpleModel(2, "string2", false)),
              List(
                TestData.SimpleModel(31, "string31", true),
                TestData.SimpleModel(32, "string32", false)
              ),
              NonEmptyList.of(
                TestData.SimpleModel(41, "string41", false),
                TestData.SimpleModel(42, "string42", true)
              )
            )

          val xml1: NodeSeq =
            <xml>
              <a>
                <a>1</a>
                <b>string1</b>
                <c>true</c>
              </a>
              <b>
                <a>2</a>
                <b>string2</b>
                <c>false</c>
              </b>
              <c>
                <a>31</a>
                <b>string31</b>
                <c>true</c>
              </c>
              <c>
                <a>32</a>
                <b>string32</b>
                <c>false</c>
              </c>
              <d>
                <a>41</a>
                <b>string41</b>
                <c>false</c>
              </d>
              <d>
                <a>42</a>
                <b>string42</b>
                <c>true</c>
              </d>
            </xml>

          val complexModel2: TestData.ComplexModel =
            TestData.ComplexModel(
              TestData.SimpleModel(1, "string1", true),
              None,
              List(
                TestData.SimpleModel(31, "string31", true),
                TestData.SimpleModel(32, "string32", false)
              ),
              NonEmptyList.of(
                TestData.SimpleModel(41, "string41", false),
                TestData.SimpleModel(42, "string42", true)
              )
            )

          val xml2: NodeSeq =
            <xml>
              <a>
                <a>1</a>
                <b>string1</b>
                <c>true</c>
              </a>
              <c>
                <a>31</a>
                <b>string31</b>
                <c>true</c>
              </c>
              <c>
                <a>32</a>
                <b>string32</b>
                <c>false</c>
              </c>
              <d>
                <a>41</a>
                <b>string41</b>
                <c>false</c>
              </d>
              <d>
                <a>42</a>
                <b>string42</b>
                <c>true</c>
              </d>
            </xml>

          val complexModel3: TestData.ComplexModel =
            TestData.ComplexModel(
              TestData.SimpleModel(1, "string1", true),
              None,
              List.empty,
              NonEmptyList.of(
                TestData.SimpleModel(41, "string41", false),
                TestData.SimpleModel(42, "string42", true)
              )
            )

          val xml3: NodeSeq =
            <xml>
              <a>
                <a>1</a>
                <b>string1</b>
                <c>true</c>
              </a>
              <d>
                <a>41</a>
                <b>string41</b>
                <c>false</c>
              </d>
              <d>
                <a>42</a>
                <b>string42</b>
                <c>true</c>
              </d>
            </xml>

          val complexModel4: TestData.ComplexModel =
            TestData.ComplexModel(
              TestData.SimpleModel(1, "string1", true),
              None,
              List.empty,
              NonEmptyList.of(
                TestData.SimpleModel(41, "string41", false)
              )
            )

          val xml4: NodeSeq =
            <xml>
              <a>
                <a>1</a>
                <b>string1</b>
                <c>true</c>
              </a>
              <d>
                <a>41</a>
                <b>string41</b>
                <c>false</c>
              </d>
            </xml>

          val complexModelTable: TableFor2[TestData.ComplexModel, NodeSeq] =
            Table(
              ("complexModel", "xml"),
              (complexModel1, xml1),
              (complexModel2, xml2),
              (complexModel3, xml3),
              (complexModel4, xml4)
            )

          forAll(complexModelTable) { (complexModel, xml) =>
            XmlOps.normalize(complexModel.toXml("xml")) shouldBe XmlOps.normalize(xml)
          }
        }
      }
    }

    ".toXmlRoot" - {

      "should write a rooted ADT with a default label" - {

        "when there is an implicit RootedXmlWriter instance available" in {
          val complexModel: TestData.ComplexModel =
            TestData.ComplexModel(
              TestData.SimpleModel(1, "string1", true),
              Some(TestData.SimpleModel(2, "string2", false)),
              List(
                TestData.SimpleModel(31, "string31", true),
                TestData.SimpleModel(32, "string32", false)
              ),
              NonEmptyList.of(
                TestData.SimpleModel(41, "string41", false),
                TestData.SimpleModel(42, "string42", true)
              )
            )

          val xml: NodeSeq =
            <ComplexModel>
              <a>
                <a>1</a>
                <b>string1</b>
                <c>true</c>
              </a>
              <b>
                <a>2</a>
                <b>string2</b>
                <c>false</c>
              </b>
              <c>
                <a>31</a>
                <b>string31</b>
                <c>true</c>
              </c>
              <c>
                <a>32</a>
                <b>string32</b>
                <c>false</c>
              </c>
              <d>
                <a>41</a>
                <b>string41</b>
                <c>false</c>
              </d>
              <d>
                <a>42</a>
                <b>string42</b>
                <c>true</c>
              </d>
            </ComplexModel>

          XmlOps.normalize(complexModel.toXmlRoot) shouldBe XmlOps.normalize(xml)
        }
      }
    }
  }
