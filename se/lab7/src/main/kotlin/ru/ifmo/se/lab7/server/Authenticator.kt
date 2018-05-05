package ru.ifmo.se.lab7.server

import org.mindrot.jbcrypt.BCrypt

const val BCRYPT_COST = 13

class Authenticator {
  companion object {
    fun passwordHash(plaintextPassword: String) =
      BCrypt.hashpw(plaintextPassword, BCrypt.gensalt(BCRYPT_COST))

    fun authenticate(login: String, password: String) =
      AUTH_TABLE[login]?.let { hash -> BCrypt.checkpw(password, hash) } ?: false
  }
}
