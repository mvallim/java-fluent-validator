package br.com.fluentvalidator;

import java.util.function.Predicate;

public interface WhenProperty<P> {

	ValidationBuilder<P> when(final Predicate<P> predicate);

}
