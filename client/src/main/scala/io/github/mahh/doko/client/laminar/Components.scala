package io.github.mahh.doko.client.laminar

import com.raquo.laminar.api.L.*
import io.github.mahh.doko.shared.msg.MessageToServer.SetUserName

object Components {

  def nameInput(
    disabledSignal: SignalSource[Boolean],
    commandSink: SetUserName => Unit
  ): Div =
    div(
      label("Your name: "),
      input(
        onMountFocus,
        placeholder := "Enter your name here",
        disabled <-- disabledSignal,
        onInput.mapToValue.map(SetUserName.apply) --> commandSink
      ),
      p()
    )
}
