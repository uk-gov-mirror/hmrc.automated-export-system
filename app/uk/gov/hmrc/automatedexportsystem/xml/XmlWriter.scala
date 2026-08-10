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

import scala.xml.*

trait XmlWriter[T]:
  def write(o: T, label: String): NodeSeq

  def contramap[U](f: U => T): XmlWriter[U] =
    (o, label) => write(f(o), label)

trait RootedXmlWriter[T] extends XmlWriter[T]:
  def defaultLabel: String

  def writeWithDefaultLabel(o: T): NodeSeq =
    write(o, defaultLabel)

object XmlWriter:
  extension [T](o: T)
    def toXml(label: String)(using writer: XmlWriter[T]): NodeSeq =
      writer.write(o, label)

    def toXmlRoot(using writer: RootedXmlWriter[T]): NodeSeq =
      writer.writeWithDefaultLabel(o)

  def elem(label: String, children: NodeSeq): Elem =
    Elem(null, label, Null, TopScope, false, children*)

  private def emptyElem(label: String): Elem =
    elem(label, NodeSeq.Empty)

  given stringWriter: XmlWriter[String] =
    (o, label) =>
      val text: String = o.trim

      if text.isEmpty then emptyElem(label)
      else elem(label, Text(text))

  given intWriter: XmlWriter[Int] =
    (o, label) => elem(label, Text(o.toString))

  given booleanWriter: XmlWriter[Boolean] =
    (o, label) => elem(label, Text(o.toString))

  given optionWriter[T](using writer: XmlWriter[T]): XmlWriter[Option[T]] =
    (o, label) => o.fold(NodeSeq.Empty)(writer.write(_, label))

  given listWriter[T](using writer: XmlWriter[T]): XmlWriter[List[T]] =
    (o, label) => o.flatMap(writer.write(_, label))

  given nonEmptyListWriter[T](using writer: XmlWriter[List[T]]): XmlWriter[NonEmptyList[T]] =
    writer.contramap(_.toList)
