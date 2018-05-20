package ru.ifmo.se.lab7.client.components

import javafx.scene.control.Button
import tornadofx.*

class NavigationHeader(expandedView: View, navigableViews: Map<String, View>): Fragment() {
  private var topLevelView = expandedView

  private val nestedNavigationPath = mutableListOf<View>()

  private val topLevelButtons: List<Button> = navigableViews.map {
    button(it.key.toUpperCase()) {
      styleClass.add("navigation-header__item")

      if (topLevelView == it.value) isDisable = true

      action {
        if (topLevelView != it.value) {
          resetTopLevel()
          isDisable = true

          topLevelView.replaceWith(it.value)
          topLevelView = it.value
        }
      }
    }
  }

  private val topLevelBox = hbox {
    topLevelButtons.forEach { add(it) }
  }

  private val nestedNavigationBox = hbox {
    button("\uf060") {
      styleClass.add("navigation-header__back")
      action(::popSubview)
    }
  }

  override val root = hbox {
    styleClass.add("navigation-header")
    add(topLevelBox)
  }

  fun navigateTo(subview: View) {
    if (nestedNavigationPath.isEmpty()) {
      topLevelBox.removeFromParent()
      root.add(nestedNavigationBox)
    }
    nestedNavigationPath.add(subview)
    topLevelView.replaceWith(subview)
  }

  private fun popSubview() {
    val popped = nestedNavigationPath.last()
    nestedNavigationPath.removeAt(nestedNavigationPath.lastIndex)
    if (nestedNavigationPath.isEmpty()) {
      nestedNavigationBox.removeFromParent()
      root.add(topLevelBox)
      popped.replaceWith(topLevelView)
    }
    else popped.replaceWith(nestedNavigationPath.last())
  }

  private fun resetTopLevel() = topLevelButtons.forEach { it.isDisable = false }
}
