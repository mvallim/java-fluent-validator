package br.com.fluentvalidator.rule;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;

import br.com.fluentvalidator.builder.Rule;

class ValidationRuleContainer<T, P> {

	private final Collection<Rule<P>> rules = new LinkedList<>();
		
	private Validation<T, P> currentValidation;
	
	protected void addRule(final Validation<T, P> validation) {
		this.currentValidation = validation;
		this.rules.add(currentValidation);
	}
	
	public Validation<T, P> getCurrentValidation() {
		return currentValidation;
	}
	
	public Collection<Rule<P>> getRules() {
		return Collections.unmodifiableCollection(rules);
	}

}
