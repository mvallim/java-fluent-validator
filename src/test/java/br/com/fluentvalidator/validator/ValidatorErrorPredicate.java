package br.com.fluentvalidator.validator;

import br.com.fluentvalidator.AbstractValidator;
import br.com.fluentvalidator.model.Bill;

import java.util.Arrays;

import static br.com.fluentvalidator.predicate.LogicalPredicate.not;
import static br.com.fluentvalidator.predicate.ObjectPredicate.nullValue;

public class ValidatorErrorPredicate extends AbstractValidator<Bill> {

    @Override
    public void rules() {


        ruleFor(bill -> bill)
                .must(not(nullValue()))
                .withMessage("Object is required")
                .withFieldName("root")

                .must(bill -> bill.getValue() > 1)
                .when(not(nullValue())).withMessage("Value must be greater than 1");


        ruleFor(bill -> bill.getDescription())
                .must(not(nullValue()))
                .when(e -> e.equalsIgnoreCase("1"));

        ruleForEach(bill -> Arrays.asList(bill.getDescription().split(",")))
                .whenever(not(nullValue()))
                .withValidator(new AbstractValidator<String>() {
                    @Override
                    public void rules() {
                        ruleFor(s -> s)
                                .must(s -> Integer.parseInt(s) > 1);
                    }
                });


    }
}
