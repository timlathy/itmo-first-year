package ru.ifmo.se.lab7.client

import javafx.beans.binding.StringBinding
import javafx.beans.property.ObjectProperty
import javafx.beans.property.SimpleObjectProperty
import javafx.beans.property.StringProperty
import java.util.*

fun StringProperty.i18n(key: String) = bind(LocaleControl.binding(key))

object LocaleControl {
  private val resources: ObjectProperty<ResourceBundle> = SimpleObjectProperty(ResourceBundle.getBundle("Messages"))

  fun change(loc: Locale) =
    resources.set(ResourceBundle.getBundle("Messages", loc))

  fun resourcesProperty() = resources

  fun getResources() = resourcesProperty().get()

  fun binding(key: String) = (object : StringBinding() {
    init { bind(resourcesProperty()) }

    override fun computeValue(): String = getResources().getString(key)
  })
}
