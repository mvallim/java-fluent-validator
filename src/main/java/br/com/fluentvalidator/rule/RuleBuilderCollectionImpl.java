package br.com.fluentvalidator.rule;

import java.util.Collection;
import java.util.LinkedList;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;

import br.com.fluentvalidator.Validator;
import br.com.fluentvalidator.builder.Code;
import br.com.fluentvalidator.builder.Critical;
import br.com.fluentvalidator.builder.FieldName;
import br.com.fluentvalidator.builder.Message;
import br.com.fluentvalidator.builder.Must;
import br.com.fluentvalidator.builder.RuleBuilderCollection;
import br.com.fluentvalidator.builder.WhenCollection;
import br.com.fluentvalidator.builder.WithValidator;
import br.com.fluentvalidator.context.ValidationContext;
import br.com.fluentvalidator.exception.ValidationException;

public class RuleBuilderCollectionImpl<T, P> extends AbstractRuleBuilder<T, Collection<P>, WhenCollection<T, P>>
        implements RuleBuilderCollection<T, P>, WhenCollection<T, P>, Rule<T> {

    private final Collection<Rule<Collection<P>>> rules = new LinkedList<>();

    private ValidationRule<P, Collection<P>> currentValidation;

    public RuleBuilderCollectionImpl(final Function<T, Collection<P>> function) {
        super(function);
    }

    @Override
    public boolean apply(final T instance) {
        return rules.stream().allMatch(rule -> Objects.nonNull(instance) && RuleProcessor.process(function.apply(instance), rule));
    }

    @Override
    public boolean support(final T instance) {
        return true;
    }

    @Override
    public WhenCollection<T, P> whenever(final Predicate<Collection<P>> whenever) {
        this.currentValidation = new ValidatorRuleInternal(whenever);
        this.rules.add(this.currentValidation);
        return this;
    }

    @Override
    public Must<T, Collection<P>, WhenCollection<T, P>> must(final Predicate<Collection<P>> must) {
        this.currentValidation = new ValidationRuleInternal(must);
        this.rules.add(this.currentValidation);
        return this;
    }

    @Override
    public Message<T, Collection<P>, WhenCollection<T, P>> withMessage(final String message) {
        this.currentValidation.withMessage(message);
        return this;
    }

    @Override
    public Code<T, Collection<P>, WhenCollection<T, P>> withCode(final String code) {
        this.currentValidation.withCode(code);
        return this;
    }

    @Override
    public FieldName<T, Collection<P>, WhenCollection<T, P>> withFieldName(final String fieldName) {
        this.currentValidation.withFieldName(fieldName);
        return this;
    }

    @Override
    public Critical<T, Collection<P>, WhenCollection<T, P>> critical() {
        this.currentValidation.critical();
        return this;
    }

    @Override
    public Critical<T, Collection<P>, WhenCollection<T, P>> critical(final Class<? extends ValidationException> clazz) {
        this.currentValidation.critical(clazz);
        return this;
    }

    @Override
    public WithValidator<T, Collection<P>, WhenCollection<T, P>> withValidator(final Validator<P> validator) {
        this.currentValidation.withValidator(validator);
        return this;
    }

    @Override
    public WhenCollection<T, P> when(final Predicate<Collection<P>> when) {
        this.currentValidation.when(when);
        return this;
    }

    class ValidationRuleInternal extends AbstractValidationRule<P, Collection<P>> {

        ValidationRuleInternal(final Predicate<Collection<P>> must) {
            super.must(must);
        }

        @Override
        public boolean support(final Collection<P> instance) {
            return Boolean.TRUE.equals(getWhen().test(instance));
        }

        @Override
        public boolean apply(final Collection<P> instance) {

            final boolean apply = getMust().test(instance);

            if (Boolean.FALSE.equals(apply)) {
                ValidationContext.get().addError(getFieldName(), getMessage(), getCode(), instance);
            }

            if (Objects.nonNull(getCriticalException()) && Boolean.FALSE.equals(apply)) {
                throw ValidationException.create(getCriticalException());
            }

            return !(Boolean.TRUE.equals(isCritical()) && Boolean.FALSE.equals(apply));
        }

    }

    class ValidatorRuleInternal extends AbstractValidationRule<P, Collection<P>> {

        ValidatorRuleInternal(final Predicate<Collection<P>> whenever) {
            super.whenever(whenever);
        }

        @Override
        public boolean support(final Collection<P> instance) {
            return Boolean.TRUE.equals(getWhenever().test(instance));
        }

        @Override
        public boolean apply(final Collection<P> instance) {

            final boolean apply = RuleProcessor.process(instance, getValidator());

            if (Objects.nonNull(getCriticalException()) && Boolean.FALSE.equals(apply)) {
                throw ValidationException.create(getCriticalException());
            }

            return !(Boolean.TRUE.equals(isCritical()) && Boolean.FALSE.equals(apply));
        }

    }

}
