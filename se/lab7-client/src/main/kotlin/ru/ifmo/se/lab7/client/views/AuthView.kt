package ru.ifmo.se.lab7.client.views

import javafx.beans.property.SimpleStringProperty
import javafx.geometry.Orientation
import tornadofx.*

import ru.ifmo.se.lab7.client.Styles.Companion.loginButton
import ru.ifmo.se.lab7.client.Styles.Companion.loginFieldset
import ru.ifmo.se.lab7.client.Styles.Companion.loginView
import ru.ifmo.se.lab7.client.controllers.MainController
import ru.ifmo.se.lab7.client.i18n

class AuthView : View() {
  val mainController: MainController by inject()

  private val model = object : ViewModel() {
    val username = bind { SimpleStringProperty() }
    val password = bind { SimpleStringProperty() }
  }

  init {
    titleProperty.i18n("auth.title")
  }

  override val root = form {
    addClass(loginView)

    fieldset(labelPosition = Orientation.VERTICAL) {
      addClass(loginFieldset)

      field {
        labelProperty.i18n("auth.username")
        textfield(model.username) { whenDocked { requestFocus() } }.required()
      }
      field {
        labelProperty.i18n("auth.password")
        passwordfield(model.password).required()
      }
    }

    button {
      textProperty().i18n("auth.login")
      addClass(loginButton)

      isDefaultButton = true

      action { with(model) { commit {
        mainController.login(username.value, password.value)
      }}}
    }
  }

  override fun onDock() {
    model.validate(decorateErrors = false)
  }

  fun clear() {
    model.username.value = ""
    model.password.value = ""
  }
}
