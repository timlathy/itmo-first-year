package ru.ifmo.se.lab7.server.ui

import ru.ifmo.se.lab7.server.Authenticator
import java.awt.FlowLayout
import java.awt.event.ActionEvent
import java.util.*
import javax.swing.*
import javax.swing.event.EventListenerList

/**
 * A dialog shown on startup. Performs authentication and allows the user
 * to specify server configuration (such as the port it binds to).
 */
class StartupDialog: JDialog() {
  /**
   * An event triggered after successful authentication. Contains the username
   * as well as the specified server configuration.
   */
  class SetupFinishEvent(source: Any, val user: String, val port: Int): EventObject(source)

  interface SetupFinishEventListener: EventListener {
    fun onSetupFinish(e: SetupFinishEvent)
  }

  /*
   * Event dispatching routines
   */

  val eventListeners = EventListenerList()

  fun addSetupFinishListener(listener: SetupFinishEventListener) =
    eventListeners.add(SetupFinishEventListener::class.java, listener)

  private fun triggerSetupFinishEvent(e: SetupFinishEvent) =
    eventListeners.getListeners(SetupFinishEventListener::class.java).forEach { it.onSetupFinish(e) }

  /*
   * Form objects
   */

  val labelUsername = JLabel("Username")
  val fieldUsername = JTextField(20)
  val labelPassword = JLabel("Password")
  val fieldPassword = JPasswordField(20)
  val labelServerPort = JLabel("Server port")
  val fieldServerPort = JSpinner(SpinnerNumberModel(8080, 1024, 65535, 1))
  val buttonStart = JButton("Start server").apply {
    addActionListener(loginActionHandler())
  }
  val labelLoginFailure = JLabel("Incorrect login or password").apply {
    isVisible = false
  }
  val panelLogin = JPanel(FlowLayout(FlowLayout.RIGHT)).apply {
    add(labelLoginFailure)
    add(buttonStart)
  }

  init {
    contentPane.layout = GroupLayout(contentPane).apply {
      autoCreateGaps = true
      autoCreateContainerGaps = true

      setHorizontalGroup(
        createParallelGroup(GroupLayout.Alignment.LEADING)
          .addGroup(createSequentialGroup()
            .addGroup(createParallelGroup(GroupLayout.Alignment.LEADING)
              .addComponent(labelUsername)
              .addComponent(labelPassword)
              .addComponent(labelServerPort))
            .addGroup(createParallelGroup(GroupLayout.Alignment.TRAILING)
              .addComponent(fieldUsername)
              .addComponent(fieldPassword)
              .addComponent(fieldServerPort)))
          .addComponent(panelLogin)
      )
      setVerticalGroup(
        createSequentialGroup()
          .addGroup(createParallelGroup(GroupLayout.Alignment.BASELINE)
            .addComponent(labelUsername)
            .addComponent(fieldUsername))
          .addGroup(createParallelGroup(GroupLayout.Alignment.BASELINE)
            .addComponent(labelPassword)
            .addComponent(fieldPassword))
          .addGroup(createParallelGroup(GroupLayout.Alignment.BASELINE)
            .addComponent(labelServerPort)
            .addComponent(fieldServerPort))
          .addComponent(panelLogin)
      )
    }

    pack()
    setLocationRelativeTo(null)

    title = "Server Startup"
    isVisible = true
    defaultCloseOperation = JFrame.DISPOSE_ON_CLOSE
  }

  private fun loginActionHandler(): ((ActionEvent) -> Unit) = { e ->
    if (Authenticator.authenticate(fieldUsername.text, String(fieldPassword.password))) {
      triggerSetupFinishEvent(SetupFinishEvent(e, fieldUsername.text, fieldServerPort.value as Int))
      dispose()
    }
    else {
      labelLoginFailure.isVisible = true
      fieldPassword.text = ""
    }
  }
}
