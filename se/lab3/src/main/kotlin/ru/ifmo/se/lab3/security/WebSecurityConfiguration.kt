package ru.ifmo.se.lab3.security

import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder
import org.springframework.security.config.annotation.web.builders.HttpSecurity
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter
import org.springframework.security.config.http.SessionCreationPolicy
import org.springframework.security.core.userdetails.UserDetailsService

@EnableWebSecurity
class WebSecurityConfiguration(private val userDetailsService: UserDetailsService): WebSecurityConfigurerAdapter() {
  override fun configure(http: HttpSecurity) {
    http.cors().and().csrf().disable().authorizeRequests()
      .anyRequest().authenticated()
      .and()
      .addFilter(JwtAuthFilter(authenticationManager()))
      .sessionManagement().sessionCreationPolicy(SessionCreationPolicy.STATELESS)
  }

  override fun configure(auth: AuthenticationManagerBuilder) {
    auth.userDetailsService(userDetailsService)
  }
}
