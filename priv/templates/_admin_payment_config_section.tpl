{% if m.acl.is_allowed.use.mod_admin_config %}
    {% with m.payment_config.fields[module] as fields %}
        {% if fields %}
            <section id="payment-config-{{ module|escape }}" class="widget">
                <div class="widget-header">{{ title|escape }}</div>
                <div class="widget-content">
                    {% if explanation %}<p>{{ explanation|escape }}</p>{% endif %}
                    {% wire id=#form type="submit" postback={save module=module} delegate=`m_payment_config` %}
                    <form id="{{ #form }}" method="post" action="postback" class="form">
                        <fieldset {% if not m.acl.is_admin_editable %}disabled{% endif %}>
                        {% for field in fields %}
                            {% with #field|append:"-"|append:forloop.counter as field_id %}
                            <div class="form-group{% if module == `mod_payment` %}{% if field.key == `currency` or field.key == `amount` %} payment-config-default-value{% endif %}{% endif %}">
                                {% if field.type == `boolean` %}
                                    <div class="checkbox">
                                        <label for="{{ field_id|escape }}">
                                            <input id="{{ field_id|escape }}" type="checkbox" name="{{ field.key|escape }}" value="1" {% if field.value %}checked{% endif %}>
                                            {{ field.label|capfirst|escape }}
                                        </label>
                                    </div>
                                {% elseif field.input == `radio` %}
                                    <div id="{{ field_id|escape }}-label" class="control-label"><strong>{{ field.label|capfirst|escape }}</strong></div>
                                    <div role="radiogroup" aria-labelledby="{{ field_id|escape }}-label">
                                        {% for option in field.options %}
                                            <label class="radio-inline" for="{{ field_id|escape }}-{{ option|escape }}">
                                                <input id="{{ field_id|escape }}-{{ option|escape }}" type="radio" name="{{ field.key|escape }}" value="{{ option|escape }}" {% if field.value == option %}checked{% endif %}>
                                                {{ option|capfirst|escape }}
                                            </label>
                                        {% endfor %}
                                    </div>
                                {% else %}
                                    <label for="{{ field_id|escape }}">{{ field.label|capfirst|escape }}</label>
                                    {% if field.is_readonly %}
                                        <input id="{{ field_id|escape }}" class="form-control" type="text" value="{{ field.value|escape }}" readonly>
                                    {% elseif field.options %}
                                        <select id="{{ field_id|escape }}" class="form-control" name="{{ field.key|escape }}">
                                            {% for option in field.options %}
                                                <option value="{{ option|escape }}" {% if field.value == option %}selected{% endif %}>{{ option|capfirst|escape }}</option>
                                            {% endfor %}
                                        </select>
                                    {% elseif field.is_secret %}
                                        <div class="input-group">
                                            <input id="{{ field_id|escape }}" class="form-control" type="password" name="{{ field.key|escape }}" value="{{ field.value|escape }}" autocomplete="new-password">
                                            {% include "_password_visibility.tpl" target_id=field_id %}
                                        </div>
                                    {% else %}
                                        <input id="{{ field_id|escape }}" class="form-control" type="{% if field.type == `integer` %}number{% else %}text{% endif %}" name="{{ field.key|escape }}" value="{{ field.value|escape }}" {% if field.type == `integer` %}min="0" step="1"{% endif %}>
                                    {% endif %}
                                {% endif %}
                                <p class="help-block">{{ field.description|escape }}</p>
                            </div>
                            {% endwith %}
                        {% endfor %}
                        </fieldset>
                        <div class="form-actions form-group">
                            <button type="submit" class="btn btn-primary" {% if not m.acl.is_admin_editable %}disabled{% endif %}>{_ Save _}</button>
                        </div>
                    </form>
                </div>
            </section>
        {% endif %}
    {% endwith %}
{% endif %}
