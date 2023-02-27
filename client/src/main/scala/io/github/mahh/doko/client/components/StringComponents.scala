package io.github.mahh.doko.client.components

import com.raquo.laminar.api.L.*
import io.github.mahh.doko.shared.msg.MessageToServer.SetUserName

object StringComponents {

  def nameInput(
    disabledSignal: SignalSource[Boolean],
    commandSink: SetUserName => Unit
  ): Div =
    div(
      label(
        "Your name: ",
        hidden <-- disabledSignal
      ),
      input(
        onMountFocus,
        placeholder := "Enter your name here",
        hidden <-- disabledSignal,
        onInput.mapToValue.map(SetUserName.apply) --> commandSink
      ),
      p()
    )
}
