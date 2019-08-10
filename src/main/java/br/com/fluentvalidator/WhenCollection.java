package br.com.fluentvalidator;

import java.util.Collection;
import java.util.function.Predicate;

public interface WhenCollection<P> {

	ValidationBuilder<P> when(final Predicate<Collection<P>> predicate);

}
