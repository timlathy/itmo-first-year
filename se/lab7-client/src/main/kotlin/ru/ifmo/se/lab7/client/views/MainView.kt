package ru.ifmo.se.lab7.client.views

import javafx.geometry.Side
import ru.ifmo.se.lab7.client.controllers.MainController
import tornadofx.*
import javafx.geometry.Pos
import javafx.scene.image.ImageView
import javafx.scene.layout.Background
import javafx.scene.layout.BackgroundFill
import javafx.scene.layout.Pane
import javafx.scene.paint.Color
import ru.ifmo.se.lab7.client.components.PannableCanvas

class MainView : View("EmploymentRequest Manager") {
  val mainController: MainController by inject()

  override val root = drawer(side = Side.TOP) {
    item("Dashboard") {

    }
    item("Map View", expanded = true) {
      add(MapView().apply { displayItems() })
    }
  }
}
