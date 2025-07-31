{% include "header.tpl" %}
{% if name == "Bob" %}
  Hello, Supreme Overlord {{ name | upper  }}!
{% else %}
  Hello, {{ name | upper }}!
{% endif %}

Languages:
{% for lang in languages %}
- {{ lang | upper }}
{% endfor %}
{% include "footer.tpl" %}