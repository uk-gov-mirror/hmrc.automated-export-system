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

import play.api.mvc.{ActionRefiner, Result}
import uk.gov.hmrc.automatedexportsystem.controllers.actions.request.{AesIE507Request, ValidatedXmlRequest}
import uk.gov.hmrc.automatedexportsystem.errors.XmlFailedReadError
import uk.gov.hmrc.automatedexportsystem.models.IE507.aes.AesIE507Message
import uk.gov.hmrc.automatedexportsystem.models.responses.AesErrorResponse.toErrorResponse
import uk.gov.hmrc.automatedexportsystem.xml.XmlReader.as

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import scala.xml.NodeSeq

@Singleton
class AesIE507ActionRefiner @Inject() ()(using protected val executionContext: ExecutionContext)
    extends ActionRefiner[ValidatedXmlRequest, AesIE507Request]:
  protected def refine[A](request: ValidatedXmlRequest[A]): Future[Either[Result, AesIE507Request[A]]] =
    val xml: NodeSeq = request.xml

    Future.successful(
      xml
        .as[AesIE507Message]
        .bimap(
          errors => XmlFailedReadError(errors).toErrorResponse.toResult,
          AesIE507Request(_, request.eori, request)
        )
        .toEither
    )
