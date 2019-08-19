package br.com.fluentvalidator.rule;

import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;

import br.com.fluentvalidator.builder.Rule;
import br.com.fluentvalidator.builder.RuleCollection;
import br.com.fluentvalidator.builder.WhenCollection;

public class CollectionRule<T, P> implements RuleCollection<T, P> {

	private final Function<T, Collection<P>> collectionFunction;

	private final List<ValidationHandler<Collection<P>>> validationHandlers  = new LinkedList<>(Arrays.asList(new ValidationHandler<Collection<P>>(new ValidationPropertyRule<Collection<P>>())));
	
	private final Function<List<ValidationHandler<Collection<P>>>, ValidationHandler<Collection<P>>> last = vh -> vh.get(vh.size() - 1);
	
	private final Function<List<ValidationHandler<Collection<P>>>, ValidationHandler<Collection<P>>> first = vh -> vh.get(0);
	
	public CollectionRule(final Function<T, Collection<P>> collectionFunction) {
		this.collectionFunction = collectionFunction;
	}

	@Override
	public void addRule(final Predicate<Collection<P>> predicate, final Rule<Collection<P>> rule) {
		this.validationHandlers.add(last.apply(validationHandlers).setNextHandler(new ValidationHandler<>(predicate, rule)));
	}
	
	@Override
	public WhenCollection<T, P> when(final Predicate<Collection<P>> when) {
		return new InternalRuleCollectionBuilder<>(this, when);
	}

	@Override
	public boolean apply(final T instance) {
		return ValidationProcessor.process(this.collectionFunction.apply(instance), first.apply(validationHandlers));
	}

}
