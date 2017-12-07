package ru.ifmo.se.lab3

import org.springframework.stereotype.Service
import org.springframework.security.core.userdetails.User
import org.springframework.security.core.userdetails.UserDetails
import org.springframework.security.core.userdetails.UserDetailsService

@Service
class UserDetailsServiceImpl: UserDetailsService {
  override fun loadUserByUsername(name: String) = User(name, "", arrayListOf())
}
