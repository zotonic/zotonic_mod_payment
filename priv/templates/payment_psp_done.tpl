{% extends "base.tpl" %}

{#
Page to show when returned from PSP

- Url is "/payment/<payment-nr>"
- Uses controller_template
- Check status of payment, show 'paid', 'error', or 'pending' page.
- Those statuses are blocks, so easy to overrule.

(Note that any connected actions are already performed, as a result of payment_status notifications)

TODO: we need a continuation url, optionally specify this when starting the payment
#}

{% block content %}
    {% with m.payment.status[q.payment_nr] as payment %}
        {% if not payment %}
            <p class="alert alert-error">{_ Unknown payment _}</p>
        {% elseif payment.is_paid %}
            {% block payment_paid %}
                <p>
                    {_ Thank you for your payment! _}
                </p>
            {% endblock %}
        {% elseif payment.is_failed %}
            {% block payment_failed %}
                <p>
                    {_ Your payment was not handled. _}
                </p>
            {% endblock %}
        {% else %}
            {% block payment_pending %}
                <p>
                    {_ Your payment is pending. _}
                </p>
            {% endblock %}
        {% endif %}
    {% endwith %}
{% endblock %}

