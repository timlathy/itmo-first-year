package ru.ifmo.se.lab7.client.controllers

import ru.ifmo.se.lab7.client.Server
import ru.ifmo.se.lab7.client.views.AuthView
import ru.ifmo.se.lab7.client.views.MainView
import tornadofx.*

class MainController: Controller() {
  val dataController: EmploymentRequestController by inject()

  val authView: AuthView by inject()
  val mainView: MainView by inject()

  var server: Server? = null;

  fun init(server: Server) {
    this.server = server;

    primaryStage.width = 1000.0
    primaryStage.height = 800.0
    primaryStage.centerOnScreen()

    //showLoginView()
    showMainView()
  }

  fun showLoginView() {
    mainView.replaceWith(authView, centerOnScreen = true)
  }

  fun showMainView() {
    authView.replaceWith(mainView, centerOnScreen = true)
  }

  fun login(username: String, password: String) {
    runAsync { server?.authenticate(username, password) } ui { success ->
      if (success == true) {
        authView.clear();
        showMainView();
      }
      else showLoginView();
    }
  }

  fun logout() {
    showLoginView()
  }
}
