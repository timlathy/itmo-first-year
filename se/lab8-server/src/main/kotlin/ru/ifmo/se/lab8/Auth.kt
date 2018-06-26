package ru.ifmo.se.lab8

import org.mindrot.jbcrypt.BCrypt
import java.util.*

const val BCRYPT_COST = 13

object Auth {
  private var authenticator: ((String, String) -> Boolean)? = BCrypt::checkpw

  fun passwordHash(plaintextPassword: String) =
    BCrypt.hashpw(plaintextPassword, BCrypt.gensalt(BCRYPT_COST))

  fun authenticateHeader(basicAuth: String?): Boolean =
    if (authenticator == null) true
    else basicAuth
      ?.let { it.replace("Basic ", "") }
      ?.let { String(Base64.getDecoder().decode(it)) }
      ?.let { creds ->
        val (login, password) = creds.split(":")
        Auth.authenticate(login, password)
      } ?: false

  fun authenticate(login: String?, password: String?): Boolean =
    if (authenticator == null) true
    else if (login.isNullOrBlank() || password.isNullOrBlank()) false
    else AUTH_TABLE[login]?.let { hash -> authenticator!!(password!!, hash) } ?: false

  fun disableAuthTestingOnly() {
    authenticator = null
  }
}
