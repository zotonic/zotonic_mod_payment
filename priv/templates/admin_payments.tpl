{% extends "admin_base.tpl" %}

{% block title %}{_ Payments _}{% endblock %}

{% block content %}
    <div class="admin-header">
        <h2>{_ Payments _}</h2>
    </div>
    {% if m.acl.is_allowed.use.mod_admin_config or m.acl.is_allowed.use.mod_payment %}
        <div class="well z-button-row">
            <a name="content-pager"></a>

            <form id="payment-filter-form"
                  class="form-inline pull-left"
                  method="get"
                  action="{% url payments_admin_overview %}">
                <label class="sr-only" for="payment-search">{_ Payment id or number _}</label>
                <div class="form-group">
                    <input id="payment-search"
                           type="text"
                           class="form-control"
                           name="qpayment"
                           value="{{ q.qpayment|escape }}"
                           placeholder="{_ Payment id or number _}">
                </div>
                <div class="form-group">
                    <label class="sr-only" for="payment-reference-filter">{_ Reference prefix _}</label>
                    <input id="payment-reference-filter"
                           class="form-control"
                           type="text"
                           name="qreference"
                           value="{{ q.qreference|escape }}"
                           placeholder="{_ Reference prefix _}">
                </div>
                <div class="form-group">
                    <label class="sr-only" for="payment-year-filter">{_ Year _}</label>
                    <select id="payment-year-filter" class="form-control" name="qyear">
                        <option value="">{_ All years _}</option>
                        {% for year in m.payment.years %}
                            <option value="{{ year }}"
                                {% if q.qyear|to_integer == year %}selected{% endif %}>
                                {{ year }}
                            </option>
                        {% endfor %}
                    </select>
                </div>
                <button class="btn btn-primary" type="submit">{_ Filter _}</button>
                {% if q.qpayment or q.qreference or q.qyear %}
                    <a class="btn btn-default" href="{% url payments_admin_overview %}">{_ Clear _}</a>
                {% endif %}
            </form>

            <div class="pull-right">
                <a class="btn btn-primary"
                   href="{% url export_payments_csv qpayment=q.qpayment qreference=q.qreference qyear=q.qyear %}">
                    {_ Export selection _}
                </a>

                {% button
                    class="btn btn-primary"
                    text=_"Sync new &amp; pending"
                    postback={sync_pending}
                    delegate=`mod_payment`
                %}
            </div>
        </div>

        {% with m.search.paged.payments::%{
                    payment: q.qpayment,
                    reference: q.qreference,
                    year: q.qyear,
                    page: q.page,
                    pagelen: 20
                }
                as result
        %}
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
                            <th width="12%">
                                {_ Reference _}
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
                                {{ payment.reference|escape }}
                            </td>
                            <td class="clickable">
                                {{ payment.description|escape }}
                            </td>
                            <td style="text-align: right;" class="clickable">
                                {{ payment.currency|replace:"EUR":"€" }}&nbsp;{{ payment.amount|format_price }}
                            </td>
                            <td class="clickable">
                                {% if payment.user_id %}
                                    <a href="{% url admin_edit_rsc id=payment.user_id %}">
                                        {{ payment.name_first|escape }} {{ payment.name_surname_prefix|escape }} {{ payment.name_surname|escape }}
                                    </a>
                                {% else %}
                                    {{ payment.name_first|escape }} {{ payment.name_surname_prefix|escape }} {{ payment.name_surname|escape }}
                                {% endif %}
                            </td>
                            <td class="clickable">
                                {% if payment.user_id %}
                                    <a href="{% url admin_edit_rsc id=payment.user_id %}">
                                        {{ payment.email|escape }}
                                    </a>
                                {% else %}
                                    {{ payment.email|escape }}
                                {% endif %}
                            </td>
                            <td class="clickable">
                                {{ payment.phone|escape }}
                            </td>
                        {% endblock %}
                    </tr>
                {% empty %}
                    <tr>
                        <td colspan="8">
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
