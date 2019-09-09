package br.com.fluentvalidator.rule;

import java.util.Collection;

import br.com.fluentvalidator.builder.Rule;

public final class ValidationProcessor {

	private ValidationProcessor() {
		super();
	}
	
	public static <E> boolean process(final E value, final Rule<E> rule) {
		return rule.apply(value);
	}
	
	public static <E> boolean process(final Collection<E> values, final Rule<E> rule) {
		for (final E value : values) {
			if (!process(value, rule)) return false;
		}
		return true;
	}
	
	public static <E> boolean process(final E value, final Collection<Rule<E>> rules) {
		for (final Rule<E> rule : rules) {
			if (!process(value, rule)) return false;
		}			
		return true;
	}

}
