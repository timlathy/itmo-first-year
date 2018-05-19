package ru.ifmo.se.lab7.client.views

import javafx.geometry.Side
import ru.ifmo.se.lab7.client.controllers.MainController
import tornadofx.*
import javafx.geometry.Pos
import javafx.scene.layout.Background
import javafx.scene.layout.BackgroundFill
import javafx.scene.layout.Pane
import javafx.scene.paint.Color
import ru.ifmo.se.lab7.client.components.PanZoomPane

class MainView : View("EmploymentRequest Manager") {
  val mainController: MainController by inject()

  override val root = drawer(side = Side.TOP) {
    item("Dashboard") {

    }
    item("Map View", expanded = true) {
      val pane = Pane()
      val scroll = PanZoomPane(pane)

      add(scroll)

      pane.apply {
        alignment = Pos.BOTTOM_RIGHT
        add(button {
          styleClass.addAll("object-pin", "object-pin--orange")
          layoutX = 875.0
          layoutY = 270.0
        })

        add(button {
          styleClass.addAll("object-pin", "object-pin--orange")
          layoutX = 1055.0
          layoutY = 370.0
        })

        background = Background(BackgroundFill(Color.WHITE, null, null))

        prefWidth = 2000.0
        prefHeight = 2000.0
      }
    }
  }
}
