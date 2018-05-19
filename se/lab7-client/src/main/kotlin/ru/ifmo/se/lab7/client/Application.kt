package ru.ifmo.se.lab7.client

import javafx.stage.Stage
import ru.ifmo.se.lab7.client.controllers.MainController
import ru.ifmo.se.lab7.client.views.AuthView
import tornadofx.*

class ClientApp: App(AuthView::class, Styles::class) {
  val mainController: MainController by inject()

  override fun start(stage: Stage) {
    super.start(stage)
    FX.stylesheets.add(resources["/styles.css"])

    val server = Server()

    mainController.init(server)
  }
}

fun main(args: Array<String>) {
  launch<ClientApp>(args)
}
