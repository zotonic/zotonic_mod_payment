{% with m.payment[q.payment_nr] as p %}
    <table class="table">
        <tr>
            <th>{_ Status _}</th>
            <td>
                {{ p.status|escape }} &nbsp;
                <span class="text-muted">{{ p.status_date|date:"Y-m-d H:i" }}</span>
            </td>
        </tr>
        <tr>
            <th>{_ Amount _}</th>
            <td>
                {{ p.currency|replace:"EUR":"€"|escape }} {{ p.amount|format_price }}

                {% if p.is_recurring_start %}
                    &nbsp; <span class="badge">{_ Recurring _}</span>
                {% endif %}
            </td>
        </tr>
        <tr>
            <th>{_ Description _}</th>
            <td>
                {% if p.description_html %}
                    {{ p.description_html }}
                {% else %}
                    {{ p.description|escape }}
                {% endif %}
            </td>
        </tr>
        <tr>
            <th>{_ Date _}</th>
            <td>
                {{ p.created|date:"Y-m-d H:i" }}
            </td>
        </tr>
        <tr>
            <th>{_ Name _}</th>
            <td>
                {% if p.user_id %}
                    <a href="{% url admin_edit_rsc id=p.user_id %}">{% include "_name.tpl" id=p.user_id %}</a>
                {% else %}
                    {{ p.name_first|escape }} {{ p.name_surname_prefix|escape }} {{ p.name_surname|escape }}
                {% endif %}
            </td>
        </tr>
        <tr>
            <th>{_ Address _}</th>
            <td>
                {{ p.address_street_1|escape }}<br>
                {% if p.address_street_2 %}{{ p.address_street_2|escape }}<br>{% endif %}
                {{ p.address_postcode|escape }}  {{ p.address_city|escape }}<br>
                {{ p.address_country|escape }}
            </td>
        </tr>
        <tr>
            <th>{_ Email _}</th>
            <td>
                <a href="mailto:{{ p.email|escape }}">{{ p.email|escape }}</a>
            </td>
        </tr>
        <tr>
            <th>{_ Phone _}</th>
            <td>
                {{ p.phone|escape }}
            </td>
        </tr>
        <tr>
            <th>{_ PSP _}</th>
            <td>
                {% if p.psp_module and p.psp_external_id %}
                    <a href="{% url payment_psp_detail payment_nr=p.payment_nr %}"
                       class="btn btn-primary btn-xs" target="payment-psp">
                        <span class="glyphicon glyphicon-new-window"></span> {_ view at PSP _}
                    </a>
                    &nbsp; <span class="text-muted">{{ p.psp_module }}</span>
                {% else %}
                    <span class="text-muted">
                        {_ No PSP _}
                    </span>
                {% endif %}
            </td>
        </tr>
        {% for k,v in p.props %}
            <tr>
                <th>
                    {{ k }}
                </th>
                <td>
                    {{ v|escape }}
                </td>
            </tr>
        {% endfor %}
    </table>

    {% comment %}{% print p %}{% endcomment %}

    {% wire id="payment-status-update"
            type="submit"
            postback={update_status payment_id=p.id}
            delegate=`mod_payment`
    %}
    <form class="form" action="postback" id="payment-status-update">

        <div class="form-group">
            <label>{_ Change status _}</label>
            <select name="status" class="form-control">
                {% for s in [
                        "new",
                        "pending",
                        "paid",
                        "cancelled",
                        "failed",
                        "refunded",
                        "error"
                   ]
                %}
                    <option {% if p.status == s %}selected{% endif %} value="{{ s }}">{{ s }}</option>
                {% endfor %}
            </select>
            <p class="help-block">
                {_ Changing the status can make the system inconsistent as the external PSP will not be in sync anymore. _}
            </p>
        </div>

        {% comment %}{% print p %}{% endcomment %}

        <div class="modal-footer">
            {% button class="btn btn-default" action={dialog_close} text=_"Close" tag="a" %}
            <button class="btn btn-primary" type="submit">{_ Save _}</button>
        </div>

    </form>

{% endwith %}
