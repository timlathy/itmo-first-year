package ru.ifmo.se.lab3.security

import javax.servlet.FilterChain
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse

import org.springframework.security.web.authentication.www.BasicAuthenticationFilter
import org.springframework.security.authentication.AuthenticationManager
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken
import org.springframework.security.core.context.SecurityContextHolder
import org.springframework.security.core.authority.SimpleGrantedAuthority
import org.springframework.security.core.GrantedAuthority

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
    abstract class Token() {
      abstract val user: String?
      
      abstract val roles: Collection<GrantedAuthority>
    }

    val tokenString = authHeader.replaceFirst(JWT_HEADER_PREFIX, "")
    val rawToken = Jwts.parser().setSigningKey(JWT_SECRET.toByteArray()).parseClaimsJws(tokenString)
    val subject = rawToken.getBody().getSubject()
    
    val token = when (subject) {
      JWT_SUB_BIG_BROTHER ->
        object : Token() {
          override val user = null
          
          override val roles = arrayListOf(SimpleGrantedAuthority("ROLE_BIG_BROTHER"))
        }
      else ->
        object : Token() {
          override val user = subject.replaceFirst(JWT_SUB_DEVICE_PREFIX, "")

          override val roles = arrayListOf(SimpleGrantedAuthority("ROLE_SURV_DEVICE"))
        }
    }
    
    return UsernamePasswordAuthenticationToken(token.user, null, token.roles)
  }
}
