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
import uk.gov.hmrc.automatedexportsystem.controllers.actions.request.{NotificationRequest, NotificationXmlPayloadRequest}
import uk.gov.hmrc.automatedexportsystem.errors.XmlFailedReadError
import uk.gov.hmrc.automatedexportsystem.models.notification.AesDigitalNotification
import uk.gov.hmrc.automatedexportsystem.models.responses.AesErrorResponse.toErrorResponse
import uk.gov.hmrc.automatedexportsystem.xml.XmlReader.as

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import scala.xml.NodeSeq

@Singleton
class NotificationActionRefiner @Inject() ()(using override protected val executionContext: ExecutionContext)
    extends ActionRefiner[NotificationXmlPayloadRequest, NotificationRequest]:

  override protected def refine[A](request: NotificationXmlPayloadRequest[A]): Future[Either[Result, NotificationRequest[A]]] =
    val xml: NodeSeq = request.xml

    Future.successful(
      xml
        .as[AesDigitalNotification]
        .bimap(
          errors => XmlFailedReadError(errors).toErrorResponse.toResult,
          NotificationRequest(_, request)
        )
        .toEither
    )
