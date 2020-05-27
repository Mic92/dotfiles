import web2ldapcnf
import web2ldapcnf.hosts
from web2ldap.app.cnf import Web2LDAPConfig

web2ldapcnf.access_allowed["_"] = ['0.0.0.0/0.0.0.0', '::0/0']
web2ldapcnf.hosts.ldap_uri_list = [(
    'ldap://localhost',
    'Local LDAP server on port 389',
)]
web2ldapcnf.hosts.ldap_def['ldap://localhost'] = Web2LDAPConfig(
    description='Eve ldap',
)
