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

package uk.gov.hmrc.automatedexportsystem.controllers.actions

import play.api.mvc.{ActionRefiner, AnyContentAsXml, Result, WrappedRequest}
import uk.gov.hmrc.automatedexportsystem.controllers.actions.request.{AesAuthRequest, AesXmlPayloadRequest, NotificationXmlPayloadRequest, XmlRequest}
import uk.gov.hmrc.automatedexportsystem.errors.RequestError
import uk.gov.hmrc.automatedexportsystem.models.responses.AesErrorResponse
import uk.gov.hmrc.automatedexportsystem.models.responses.AesErrorResponse.toErrorResponse

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import scala.xml.NodeSeq

// TODO (not urgent) - change to mapper injection based implementation
sealed trait XmlPayloadActionRefiner[
  T[_] <: WrappedRequest[_],
  U[_] <: WrappedRequest[_] & XmlRequest
](using protected val executionContext: ExecutionContext)
    extends ActionRefiner[T, U]:

  override protected def refine[A](request: T[A]): Future[Either[Result, U[A]]] =
    Future.successful(
      request.body match
        case xml: NodeSeq =>
          Right(createXmlRequest(request, xml))
        case anyContentAsXml: AnyContentAsXml =>
          Right(createXmlRequest(request, anyContentAsXml.xml))
        case _ =>
          val error:         RequestError     = RequestError.ExpectedXmlBodyError
          val errorResponse: AesErrorResponse = error.toErrorResponse

          Left(errorResponse.toResult)
    )

  protected def createXmlRequest[A](request: T[A], xml: NodeSeq): U[A]

@Singleton
class AesXmlPayloadActionRefiner @Inject() ()(using protected val ec: ExecutionContext)
    extends XmlPayloadActionRefiner[AesAuthRequest, AesXmlPayloadRequest]:
  protected def createXmlRequest[A](request: AesAuthRequest[A], xml: NodeSeq): AesXmlPayloadRequest[A] =
    AesXmlPayloadRequest(xml, request, request.eori)

@Singleton
class NotificationXmlPayloadActionRefiner @Inject() ()(using protected val ec: ExecutionContext)
    extends XmlPayloadActionRefiner[ValidatedNotificationRequest, NotificationXmlPayloadRequest]:
  protected def createXmlRequest[A](
    request: ValidatedNotificationRequest[A],
    xml:     NodeSeq
  ): NotificationXmlPayloadRequest[A] =
    NotificationXmlPayloadRequest(xml, request)
