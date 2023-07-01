package io.github.mahh.doko.client.components

import com.raquo.laminar.api.L.*

private def stringsWithLineBreaks(strings: Signal[List[String]]): Signal[List[Node]] =
  strings.map(_.flatMap(s => List(textToTextNode(s), br())))

// unicode zero-width empty space - appears to be the easiest way to enforce regular string
// height for an empty string
val fullHeightEmptyString = '\u200b'.toString

/**
 * Redirects `onClick` to `observer` if an `actionOpt` is defined.
 */
private def observeClicksWithActions[Action](
  actionOpt: Signal[Option[Action]],
  observer: Observer[Action]
): Modifier[Element] =
  new Modifier[Element]:
    override def apply(el: Element): Unit =
      val clickEventStream = new EventBus[org.scalajs.dom.MouseEvent]
      val clickActions: Observable[Action] =
        actionOpt
          .flatMap(a => clickEventStream.toObservable.map(_ => a))(SwitchStreamStrategy)
          .collect { case Some(a) => a }
      Seq(
        onClick --> clickEventStream,
        clickActions --> observer
      ).apply(el)
