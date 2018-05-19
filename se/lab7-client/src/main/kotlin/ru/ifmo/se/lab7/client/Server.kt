package ru.ifmo.se.lab7.client

class Server {
  fun authenticate(username: String, password: String): Boolean =
    if (username == "test" && password == "test") {
      /* store the secret... */
      true
    }
    else false
}
