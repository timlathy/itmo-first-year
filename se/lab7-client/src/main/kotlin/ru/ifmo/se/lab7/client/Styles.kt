package ru.ifmo.se.lab7.client

import javafx.geometry.Pos
import tornadofx.*

class Styles: Stylesheet() {
  companion object {
    val loginView by cssclass()
    val loginFieldset by cssclass()
    val loginButton by cssclass()
  }

  init {
    loginView {
      padding = box(15.px)
      vgap = 7.px
      hgap = 10.px

      alignment = Pos.CENTER
    }

    loginFieldset {
      maxWidth = 250.px
      maxHeight = 400.px
    }

    loginButton {
      prefWidth = 250.px
    }
  }
}
