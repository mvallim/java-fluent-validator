package br.com.fluentvalidator.spring.validator;

import static br.com.fluentvalidator.predicate.ComparablePredicate.greaterThanOrEqual;
import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;
import static br.com.fluentvalidator.predicate.StringPredicate.stringEmptyOrNull;

import org.springframework.stereotype.Component;

import br.com.fluentvalidator.AbstractValidator;
import br.com.fluentvalidator.model.Child;
import br.com.fluentvalidator.model.Parent;

@Component
public class ValidatorSpringChild extends AbstractValidator<Child> {

    @Override
    public void rules() {

        setPropertyOnContext("child");

        ruleFor(Child::getAge).must(not(nullValue())).withMessage("child age must be not null").withFieldName("age").critical().must(greaterThanOrEqual(5)).when(not(nullValue()))
                .withMessage("child age must be greater than or equal to 5").withFieldName("age").must(this::checkAgeConstraintChild).when(not(nullValue()))
                .withMessage("child age must be less than age parent").withFieldName("age").critical();

        ruleFor(Child::getName).must(not(stringEmptyOrNull())).withMessage("child name must be not null or empty").withFieldName("name");

    }

    private boolean checkAgeConstraintChild(final Integer age) {
        return age < getPropertyOnContext("parent", Parent.class).getAge();
    }

}
