package ru.ifmo.se.lab7.client

import javafx.scene.text.Font
import javafx.stage.Stage
import ru.ifmo.se.lab7.client.controllers.MainController
import ru.ifmo.se.lab7.client.views.AuthView
import tornadofx.*

class ClientApp: App(AuthView::class, Styles::class) {
  val mainController: MainController by inject()

  override fun start(stage: Stage) {
    super.start(stage)
    FX.stylesheets.add(resources["/styles/root.css"])
    Font.loadFont(resources["/fonts/RobotoCondensed-Regular.ttf"], 10.0)
    Font.loadFont(resources["/fonts/fa-regular-400.ttf"], 10.0)
    Font.loadFont(resources["/fonts/fa-solid-900.ttf"], 10.0)

    mainController.init()
  }
}

fun main(args: Array<String>) {
  launch<ClientApp>(args)
}
