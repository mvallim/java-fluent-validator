package br.com.fluentvalidator.rule;

import java.util.Collection;

import br.com.fluentvalidator.builder.Rule;

public final class ValidationProcessor {

	private ValidationProcessor() {
		super();
	}
	
	public static <P> boolean process(final P value, final Rule<P> rule) {
		return rule.apply(value);
	}
	
	public static <P> boolean process(final Collection<P> values, final Rule<P> rule) {
		for (final P value : values) {
			if (!process(value, rule)) return false;
		}
		return true;
	}
	
	public static <P> boolean process(final P value, final Collection<Rule<P>> rules) {
		for (final Rule<P> rule : rules) {
			if (!process(value, rule)) return false;
		}
		return true;
	}

}
