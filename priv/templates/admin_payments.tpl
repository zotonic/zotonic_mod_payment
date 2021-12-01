{% extends "admin_base.tpl" %}

{% block title %}{_ Payments _}{% endblock %}

{% block content %}
    <div class="admin-header">
        <h2>{_ Payments _}</h2>
    </div>
    {% if m.acl.is_allowed.use.mod_admin_config or m.acl.is_allowed.use.mod_payment %}
        <div class="well z-button-row">
            <a name="content-pager"></a>

            {% button
                class="btn btn-primary"
                text=_"Export"
                action={redirect dispatch="export_payments_csv"}
            %}

            {% button
                class="btn btn-primary"
                text=_"Sync new &amp; pending"
                postback={sync_pending}
                delegate=`mod_payment`
            %}
        </div>

        {% with m.search.paged[{payments page=q.page pagelen=20}] as result %}
            <table class="table table-striped do_adminLinkedTable" id="payments">
                <thead>
                    <tr>
                        {% block payment_table_head %}
                            <th width="10%">
                                {_ Date _}
                            </th>
                            <th width="5%">
                                {_ Status _}
                            </th>
                            <th width="15%">
                                {_ Description _}
                            </th>
                            <th width="10%" style="text-align: right;">
                                {_ Amount _}
                            </th>
                            <th width="20%">
                                {_ Name _}
                            </th>
                            <th width="15%">
                                {_ Email _}
                            </th>
                            <th width="15%">
                                {_ Phone _}
                            </th>
                        {% endblock %}
                    </tr>
                </thead>

                <tbody>
                {% for payment in result %}
                    <tr class="{% if payment.status == 'error' %}text-danger{% elseif payment.status == 'refunded' %}text-warning{% elseif payment.status != 'paid' %}unpublished{% endif %}" data-payment-nr="{{ payment.payment_nr }}">
                        {% block payment_table_row %}
                            <td class="clickable">
                                {{ payment.created|date:_"Y-m-d H:i" }}
                            </td>
                            <td class="clickable">
                                {{ payment.status }}
                            </td>
                            <td class="clickable">
                                {{ payment.description }}
                            </td>
                            <td style="text-align: right;" class="clickable">
                                {{ payment.currency|replace:"EUR":"€" }}&nbsp;{{ payment.amount|format_price }}
                            </td>
                            <td class="clickable">
                                <a href="{% url admin_edit_rsc id=payment.user_id %}">
                                    {{ payment.name_first }} {{ payment.name_surname_prefix }} {{ payment.name_surname }}
                                </a>
                            </td>
                            <td class="clickable">
                                <a href="{% url admin_edit_rsc id=payment.user_id %}">
                                    {{ payment.email }}
                                </a>
                            </td>
                            <td class="clickable">
                                {{ payment.phone }}
                            </td>
                            {#
                            <td>
                                <span class="pull-right buttons">
                                    <a href="#" class="btn btn-default btn-xs" target="payment-psp">{_ view at PSP _}</a>

                                    {% if payment.psp_module and payment.psp_external_id %}

                                            <a href="{% url payment_psp_detail payment_nr=payment.payment_nr %}" class="btn btn-default btn-xs" target="payment-psp">{_ view at PSP _}</a>
                                        </span>
                                    {% else %}
                                    <small class="pull-right text-muted">
                                        {_ No PSP _}
                                    </small>
                                {% endif %}
                            </td>
                            #}
                        {% endblock %}
                    </tr>
                {% empty %}
                    <tr>
                        <td colspan="5">
                            {_ No payments found. _}
                        </td>
                    </tr>
                {% endfor %}
                </tbody>
            </table>

            {% pager result=result dispatch=`payments_admin_overview` qargs hide_single_page %}
        {% endwith %}

        {% wire name="payment-info"
                action={dialog_open title=_"Payment" template="_dialog_payment_info.tpl"}
        %}

        {% javascript %}
            $('#payments tbody tr').on('click', function() {
                z_event('payment-info', { payment_nr: $(this).attr('data-payment-nr') });
            });
        {% endjavascript %}
    {% else %}
        <div class="alert alert-info">
            {_ Only administrators are allowed to see payments _}
        </div>
    {% endif %}

{% endblock %}
