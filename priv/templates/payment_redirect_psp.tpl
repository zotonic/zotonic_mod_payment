{% extends "base.tpl" %}

{% block content %}
{% with m.payment.redirect_psp[q.payment_nr] as redirect_uri %}
    <div style="padding:20px 10px;">
        {% if not redirect_uri %}
            <p class="alert alert-danger" style="max-width: 400px; margin: 0 auto;">
                {_ Unknown payment or no detail page available. _}
            </p>

            <p style="text-align: center; margin: 30px 0">
                <a href="#" id="close-btn" class="btn btn-primary">{_ Close Window _}</a>
                {% wire id="close-btn" action={script script="window.close();"} %}
            </p>
        {% else %}
            <p class="alert alert-info" style="max-width: 400px; margin: 0 auto;">
                {_ Redirecting to payment service provider... _}
            </p>

            {% wire action={redirect location=redirect_uri} %}
        {% endif %}
    </div>
{% endwith %}
{% endblock %}
