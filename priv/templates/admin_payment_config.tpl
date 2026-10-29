{% extends "admin_base.tpl" %}

{% block title %}{_ Payment configuration _}{% endblock %}

{% block head_extra %}
    {% inherit %}
    {% lib "css/payment-admin.css" %}
{% endblock %}

{% block content %}
    <div class="admin-header">
        <h2>{_ Payment configuration _}</h2>
    </div>
    {% if m.acl.is_allowed.use.mod_admin_config %}
        <p>{_ Configure payment defaults, incoming payments, notifications and the enabled payment providers. Save each section separately. _}</p>
        <p class="help-block">{_ Credentials are masked. Use the show-password button to view them, or empty a field to remove its value. _}</p>
        <div class="payment-config-sections">
            {% include "_admin_payment_config_section.tpl" module=`mod_payment` title=_"General payment settings" %}
            {% all include "_admin_payment_config_psp.tpl" %}
        </div>
    {% else %}
        <p class="alert alert-warning">{_ You are not allowed to configure payments. _}</p>
    {% endif %}
{% endblock %}
