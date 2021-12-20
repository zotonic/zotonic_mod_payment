{% extends "email_base.tpl" %}

{% block title %}{_ Received payment on _} {{ m.site.title }}{% endblock %}

{% block preheader %}
    {% trans "Payment of {currency} {amount} received."
        currency=payment.currency|replace:"EUR":"€"|escape
        amount=payment.amount|format_price
    %}
    {_ From _}:
    {% if payment.user_id %}{% include "_name.tpl" id=payment.user_id %}
    {% else %}{{ payment.name_first|escape }} {{ payment.name_surname_prefix|escape }} {{ payment.name_surname|escape }}
    {% endif %}
{% endblock %}

{% block body %}
{% with payment as p %}
    <table>
        <tr>
            <th align="left" valign="top">{_ Status _}</th>
            <td valign="top">
                {{ p.status|escape }} &nbsp;
                <span style="color:#666;">{{ p.status_date|date:"Y-m-d H:i" }}</span>
            </td>
        </tr>
        <tr>
            <th align="left" valign="top">{_ Amount _}</th>
            <td valign="top">
                {{ p.currency|replace:"EUR":"€"|escape }} {{ p.amount|format_price }}

                {% if p.is_recurring_start %}
                    &nbsp; <span class="badge">{_ Recurring _}</span>
                {% endif %}
            </td>
        </tr>
        <tr>
            <th align="left" valign="top">{_ Description _}</th>
            <td valign="top">
                {% if p.description_html %}
                    {{ p.description_html }}
                {% else %}
                    {{ p.description|escape }}
                {% endif %}
            </td>
        </tr>
        <tr>
            <th align="left" valign="top">{_ Date _}</th>
            <td valign="top">
                {{ p.created|date:"Y-m-d H:i" }}
            </td>
        </tr>
        <tr>
            <th align="left" valign="top">{_ Name _}</th>
            <td valign="top">
                {% if p.user_id %}
                    <a href="{% url admin_edit_rsc id=p.user_id absolute_url %}">{% include "_name.tpl" id=p.user_id %}</a>
                {% else %}
                    {{ p.name_first|escape }} {{ p.name_surname_prefix|escape }} {{ p.name_surname|escape }}
                {% endif %}
            </td>
        </tr>
        <tr>
            <th align="left" valign="top">{_ Address _}</th>
            <td valign="top">
                {% if p.address_street_1 %}{{ p.address_street_1|escape }}<br>{% endif %}
                {% if p.address_street_2 %}{{ p.address_street_2|escape }}<br>{% endif %}
                {% if p.address_postcode or p.address_city%}{{ p.address_postcode|escape }}  {{ p.address_city|escape }}<br>{% endif %}
                {{ m.l10n.country_name[p.address_country]|escape }}
            </td>
        </tr>
        <tr>
            <th align="left" valign="top">{_ Email _}</th>
            <td valign="top">
                {{ p.email|escape }}
            </td>
        </tr>
        {% if p.phone %}
            <tr>
                <th align="left" valign="top">{_ Phone _}</th>
                <td valign="top">
                    {{ p.phone|escape }}
                </td>
            </tr>
        {% endif %}
        <tr>
            <th align="left" valign="top">{_ Payment Service Provider _} &nbsp;</th>
            <td valign="top">
                {% if p.psp_module and p.psp_external_id %}
                    <a href="{% url payment_psp_detail payment_nr=p.payment_nr absolute_url %}"
                       class="btn btn-primary btn-xs" target="payment-psp">
                        <span class="glyphicon glyphicon-new-window"></span>
                        {% trans "view at {psp}"
                                psp= p.psp_module|to_binary|replace:"mod_payment_":""|replace:"_":" "|capfirst
                        %}
                    </a>
                {% else %}
                    <span style="color:#666;">
                        {_ No PSP _}
                    </span>
                {% endif %}
            </td>
        </tr>
        {% for k,v in p.props %}
            <tr>
                <th align="left" valign="top">
                    {{ k|replace:"_":" "|capfirst }}
                </th>
                <td valign="top">
                    {{ v|escape }}
                </td>
            </tr>
        {% endfor %}
    </table>
{% endwith %}
{% endblock %}
