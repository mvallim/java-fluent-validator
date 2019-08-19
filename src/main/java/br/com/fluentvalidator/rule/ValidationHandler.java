package br.com.fluentvalidator.rule;

import java.util.function.Predicate;

import br.com.fluentvalidator.builder.Rule;

class ValidationHandler<P> implements Rule<P> {

	private Predicate<P> predicate = p -> true;
	
	private Rule<P> rule;
	
	private ValidationHandler<P> nextHandler;

	public ValidationHandler(final Rule<P> rule) {
		this.rule = rule;
	}

	public ValidationHandler(final Predicate<P> predicate, final Rule<P> rule) {
		this.predicate = predicate;
		this.rule = rule;
	}

	@Override
	public boolean apply(final P instance) {
		return (predicate.test(instance) && rule.apply(instance)) && (nextHandler == null || nextHandler.apply(instance));
	}

	public ValidationHandler<P> setNextHandler(final ValidationHandler<P> nextHandler) {
		this.nextHandler = nextHandler;
		return nextHandler;
	}

}
