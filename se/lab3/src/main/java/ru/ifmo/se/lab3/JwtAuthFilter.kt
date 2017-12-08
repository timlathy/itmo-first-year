package ru.ifmo.se.lab3

import javax.servlet.FilterChain
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse

import org.springframework.security.web.authentication.www.BasicAuthenticationFilter
import org.springframework.security.authentication.AuthenticationManager
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken
import org.springframework.security.core.context.SecurityContextHolder
import org.springframework.security.core.authority.SimpleGrantedAuthority

import io.jsonwebtoken.Jwts
  
const val JWT_HEADER = "Authorization"
const val JWT_HEADER_PREFIX = "Bearer "
const val JWT_SECRET = "RandomSecretSharedWithTheAuthService"

const val JWT_SUB_BIG_BROTHER = "BIG_BROTHER"
const val JWT_SUB_DEVICE_PREFIX = "DEVICE_OF "

class JwtAuthFilter(private val authManager: AuthenticationManager): BasicAuthenticationFilter(authManager) {
  override fun doFilterInternal(req: HttpServletRequest, res: HttpServletResponse, chain: FilterChain) {
    val authHeader = req.getHeader(JWT_HEADER)

    if (authHeader?.startsWith(JWT_HEADER_PREFIX) ?: false) {
      SecurityContextHolder.getContext().setAuthentication(getJwtAuth(authHeader))
    }

    chain.doFilter(req, res)
  }

  fun getJwtAuth(authHeader: String): UsernamePasswordAuthenticationToken {
    val tokenString = authHeader.replaceFirst(JWT_HEADER_PREFIX, "")
    val token = Jwts.parser().setSigningKey(JWT_SECRET.toByteArray()).parseClaimsJws(tokenString)
    val subject = token.getBody().getSubject()
    
    val (user, roles) =
      if (subject.startsWith(JWT_SUB_DEVICE_PREFIX)) {
        val username = subject.replaceFirst(JWT_SUB_DEVICE_PREFIX, "")
        Pair(username, arrayListOf(SimpleGrantedAuthority("ROLE_SURV_DEVICE")))
      }
      else if (subject == JWT_SUB_BIG_BROTHER) {
        Pair("Big Brother", arrayListOf(SimpleGrantedAuthority("ROLE_BIG_BROTHER")))
      }
      else {
        Pair(subject, arrayListOf())
      }

    return UsernamePasswordAuthenticationToken(user, null, roles)
  }
}
