vault {
    renew_token = false
    vault_agent_token_file = "/home/consul-template/.vault-token"
    retry {
    backoff = "1s"
    }
}

template {
    destination = "/etc/secrets/secrets.yaml" # Everything placed in /etc/secrets will be available in a volume
    contents = <<EOH
    {{- with secret "kv/data/ejabberd/mysql" }}
    define_macro:
        'MYSQL_USERNAME': "{{ .Data.data.username }}"
        'MYSQL_PASSWORD': "{{ .Data.data.password }}"
    {{ end }}

    {{- with secret "kv/data/ejabberd/rabbitmq" }}
    define_macro:
        'RABBITMQ_AUTH': "amqp://{{ .Data.data.username }}:{{ .Data.data.password }}@rabbit.$(PUBLIC_DNS_ZONE_NAME):5672/skillz"
    {{ end }}
    EOH
}
