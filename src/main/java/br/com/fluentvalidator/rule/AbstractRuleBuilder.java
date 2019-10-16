package br.com.fluentvalidator.rule;

import java.util.function.Function;
import br.com.fluentvalidator.builder.AttemptedValue;
import br.com.fluentvalidator.builder.Code;
import br.com.fluentvalidator.builder.Critical;
import br.com.fluentvalidator.builder.FieldName;
import br.com.fluentvalidator.builder.HandleInvalidField;
import br.com.fluentvalidator.builder.Message;
import br.com.fluentvalidator.builder.Must;
import br.com.fluentvalidator.builder.When;
import br.com.fluentvalidator.builder.WithValidator;

abstract class AbstractRuleBuilder<T, P, W extends When<T, P, W>> implements When<T, P, W>, Must<T, P, W>, Message<T, P, W>, FieldName<T, P, W>, Code<T, P, W>, Critical<T, P, W>, WithValidator<T, P, W>, HandleInvalidField<T, P, W>, AttemptedValue<T, P, W>, Rule<T> {

  protected final Function<T, String> fieldName;

  protected final Function<T, P> function;

  public AbstractRuleBuilder(final Function<T, String> fieldName, final Function<T, P> function) {
    this.fieldName = fieldName;
    this.function = function;
  }

  public AbstractRuleBuilder(final String fieldName, final Function<T, P> function) {
    this(obj -> fieldName, function);
  }

  public AbstractRuleBuilder(final Function<T, P> function) {
    this(obj -> null, function);
  }

}
