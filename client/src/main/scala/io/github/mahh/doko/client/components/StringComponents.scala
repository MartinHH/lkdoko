package io.github.mahh.doko.client.components

import com.raquo.laminar.api.L.*
import io.github.mahh.doko.shared.msg.MessageToServer.SetUserName
import io.laminext.syntax.core.*

object StringComponents {

  def nameInput(
    isAllowed: Signal[Boolean],
    commandSink: Observer[SetUserName]
  ): Div =
    div(
      label(
        "Your name: "
      ),
      input(
        onMountFocus,
        placeholder := "Enter your name here",
        onInput.mapToValue.map(SetUserName.apply) --> commandSink,
        disabled <-- isAllowed.not
      ),
      p()
    )
}
