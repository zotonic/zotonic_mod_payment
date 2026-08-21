{% extends "base.tpl" %}

{% block title %}{_ Payment _}{% endblock %}

{% block html_head_extra %}
    {% lib "css/payment.css" %}
{% endblock %}

{% block content_area %}
    {% with
            q.description|default:m.payment.default.description|truncate:200|trim,
            q.reference|truncate:100|trim,
            q.amount|default:m.payment.default.amount,
            q.currency|default:m.payment.default.currency
        as
            description,
            reference,
            amount,
            currency
    %}
        <main class="payment-link-page">
            <section class="payment-link panel panel-default" aria-labelledby="payment-link-title">
                <div class="panel-body">
                    <h1 id="payment-link-title" class="h3">{_ Please confirm your payment _}</h1>

                    {% if description %}
                        <p class="payment-link__description">{{ description|escape }}</p>
                    {% endif %}

                    {% if reference %}
                        <p class="payment-link__reference text-muted">
                            <small>{% trans "Reference: <b>{reference}</b>" reference=reference|escape %}</small>
                        </p>
                    {% endif %}

                    {% if amount|payment_is_valid:currency %}
                        <p class="payment-link__amount lead">
                            {{ currency|payment_currency_symbol|escape }} {{ amount|escape }}
                        </p>

                        {% wire
                            id="payment-link-form"
                            type="submit"
                            postback={payment_link
                                description=description
                                reference=reference
                                amount=amount
                                currency=currency
                            }
                            delegate=`mod_payment`
                            action={mask target="payment-link-form" message=_"Redirecting..."}
                        %}
                        <form id="payment-link-form" method="post" action="postback">
                            <button class="btn btn-primary btn-lg payment-link__button" type="submit">
                                {_ Continue to payment _}
                            </button>
                        </form>

                        <p class="text-muted">
                            <small>
                                {_ After clicking the button, you will be redirected to the payment provider's website to complete your payment. _}
                            </small>
                        </p>
                    {% else %}
                        <p class="alert alert-danger" role="alert">
                            {_ Sorry, the payment amount or currency is not valid. _}
                        </p>
                    {% endif %}
                </div>
            </section>
        </main>
    {% endwith %}
{% endblock %}
