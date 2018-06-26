package ru.ifmo.se.lab7.client.controllers

import javafx.scene.control.Alert
import ru.ifmo.se.lab7.client.views.AuthView
import ru.ifmo.se.lab7.client.views.MainView
import tornadofx.*
import java.nio.charset.StandardCharsets.UTF_8
import java.util.*

class MainController: Controller() {
  val authView: AuthView by inject()
  val mainView: MainView by inject()

  val api: Rest by inject()

  init {
    Rest.useApacheHttpClient()
    api.baseURI = "http://localhost:8080/"
  }

  fun init() {
    primaryStage.width = 1000.0
    primaryStage.height = 840.0
    primaryStage.centerOnScreen()

    showLoginView()
  }

  fun showLoginView() {
    mainView.replaceWith(authView, centerOnScreen = true)
  }

  fun showMainView() {
    authView.replaceWith(mainView, centerOnScreen = true)
  }

  fun login(username: String, password: String) {
    runAsync {
      api.engine.requestInterceptor = { engine ->
        val b64 = Base64.getEncoder().encodeToString("$username:$password".toByteArray(UTF_8))
        engine.addHeader("Authorization", "Basic $b64")
      }
      try {
        val status = api.get("/queue").statusCode
        status == 200
      }
      catch (e: Exception) {
        e.printStackTrace()
        false
      }
    } ui { authenticated ->
      if (authenticated) {
        authView.clear()
        showMainView()
      }
      else {
        alert(Alert.AlertType.ERROR, header = "Authentication failed", content = "Incorrect username or password")
        showLoginView()
      }
    }
  }

  fun logout() {
    showLoginView()
  }
}
