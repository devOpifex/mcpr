# OAuth 2.1 Example for mcpr

This example demonstrates how to implement OAuth 2.1 authentication with MCP servers according to the Model Context Protocol specification.

## Overview

The example includes:
- An OAuth 2.1 compliant MCP server with secure calculator tool
- A test client demonstrating the complete authentication flow
- PKCE (Proof Key for Code Exchange) implementation for security
- Bearer token authentication for MCP requests

## Files

- `oauth_server.R` - MCP server with OAuth 2.1 protection
- `oauth_client_test.R` - Client test demonstrating OAuth flow
- `README.md` - This documentation

## Quick Start

### 1. Start the OAuth-protected MCP Server

```r
source("oauth_server.R")
```

This will:
- Create an MCP server with a secure calculator tool
- Setup OAuth 2.1 endpoints following MCP specification
- Generate a test client with credentials
- Start the server on `http://localhost:8080`

The server provides these endpoints:

#### OAuth Endpoints
- `/.well-known/oauth-authorization-server` - Authorization server metadata (RFC 8414)
- `/.well-known/oauth-protected-resource` - Protected resource metadata
- `/.well-known/jwks.json` - JSON Web Key Set for token verification
- `/oauth/authorize` - Authorization endpoint
- `/oauth/token` - Token endpoint
- `/oauth/register` - Dynamic client registration (RFC 7591)

#### MCP Endpoints
- `/mcp` - Protected MCP server endpoint (requires Bearer token)
- `/health` - Server health check

### 2. Test the OAuth Flow

```r
source("oauth_client_test.R")
```

This demonstrates:
1. Fetching OAuth server metadata
2. Generating PKCE challenge and authorization URL
3. Obtaining authorization code
4. Exchanging code for access token using PKCE
5. Making authenticated MCP requests with Bearer token
6. Verifying that unauthenticated requests are rejected

## OAuth 2.1 Features Implemented

### Core OAuth 2.1 Compliance
- ✅ Authorization Code flow with PKCE (required)
- ✅ JWT Bearer tokens with audience binding
- ✅ Dynamic client registration (RFC 7591)
- ✅ Authorization server metadata (RFC 8414)
- ✅ Protected resource metadata
- ✅ Token introspection (RFC 7662)
- ✅ Token revocation (RFC 7009)

### Security Features
- ✅ PKCE (S256) for all authorization code flows
- ✅ Audience validation for tokens
- ✅ Short-lived access tokens (1 hour default)
- ✅ Proper WWW-Authenticate headers
- ✅ Rate limiting on OAuth endpoints
- ✅ CORS support for web applications

### MCP-Specific Features
- ✅ Bearer token authentication for MCP requests
- ✅ Scope-based access control (`mcp:tools`, `mcp:resources`, `mcp:prompts`)
- ✅ Integration with existing MCP server architecture
- ✅ Backward compatibility (OAuth is optional)

## Configuration Options

### OAuth Server Configuration

```r
oauth_config <- new_oauth_server(
  issuer_url = "https://your-server.com",
  client_storage = oauth_file_storage("clients.json"),
  token_storage = oauth_file_storage("tokens.json"),
  config = list(
    access_token_expires_in = 3600,          # Token lifetime
    supported_scopes = c("mcp:tools"),       # Available scopes
    allow_dynamic_registration = TRUE,       # Enable client registration
    require_pkce = TRUE,                     # Require PKCE (recommended)
    rate_limit = list(
      enabled = TRUE,
      requests_per_minute = 100
    )
  )
)
```

### MCP Server with OAuth

```r
serve_http(
  mcp_server,
  port = 8080,
  oauth_config = oauth_config,
  require_auth = TRUE,                       # Require authentication
  protected_scopes = c("mcp:tools")          # Required scopes
)
```

## Storage Options

### In-Memory Storage (Development)
```r
client_storage = oauth_memory_storage()
token_storage = oauth_memory_storage()
```

### File Storage (Production)
```r
client_storage = oauth_file_storage("oauth_clients.rds")
token_storage = oauth_file_storage("oauth_tokens.rds")
```

## Integration with AI Systems

### Claude Code

Add to your MCP configuration:
```bash
claude mcp add secure-calculator --oauth -- Rscript /path/to/oauth_server.R
```

### Cursor

Configure in `.cursor/mcp.json`:
```json
{
  "servers": {
    "secure-calculator": {
      "command": "Rscript",
      "args": ["/path/to/oauth_server.R"],
      "oauth": {
        "authorization_url": "http://localhost:8080/oauth/authorize",
        "token_url": "http://localhost:8080/oauth/token",
        "scopes": ["mcp:tools", "mcp:resources"]
      }
    }
  }
}
```

## Security Considerations

1. **HTTPS Required**: In production, use HTTPS for all OAuth endpoints
2. **Secure Token Storage**: Use persistent, encrypted storage for tokens
3. **Client Secret Protection**: Store client secrets securely
4. **Token Rotation**: Implement refresh tokens for long-lived access
5. **Audit Logging**: Log all authentication events
6. **Rate Limiting**: Protect OAuth endpoints from abuse

## Troubleshooting

### Common Issues

**"Token validation failed"**
- Check token expiration
- Verify audience matches server URL
- Ensure token hasn't been revoked

**"Insufficient scope"**
- Check token includes required scopes
- Verify scope configuration matches server requirements

**"PKCE verification failed"**
- Ensure code_verifier matches code_challenge
- Check code_challenge_method is "S256"

### Debug Mode

Enable debug logging:
```r
options(mcpr.oauth.debug = TRUE)
```

This will log OAuth request details for troubleshooting.

## Standards Compliance

This implementation follows these specifications:
- [Model Context Protocol](https://modelcontextprotocol.io/specification)
- [OAuth 2.1](https://datatracker.ietf.org/doc/draft-ietf-oauth-v2-1/)
- [RFC 7636](https://tools.ietf.org/html/rfc7636) - PKCE
- [RFC 8414](https://tools.ietf.org/html/rfc8414) - Authorization Server Metadata
- [RFC 7591](https://tools.ietf.org/html/rfc7591) - Dynamic Client Registration
- [RFC 7662](https://tools.ietf.org/html/rfc7662) - Token Introspection
- [RFC 7009](https://tools.ietf.org/html/rfc7009) - Token Revocation