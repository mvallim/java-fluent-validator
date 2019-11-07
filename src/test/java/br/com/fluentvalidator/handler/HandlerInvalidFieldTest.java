package br.com.fluentvalidator.handler;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;
import java.util.Collection;
import org.junit.Test;

public class HandlerInvalidFieldTest {

  @Test
  public void testSuccessDefaultHandle() {

    final HandlerInvalidField<String> handlerInvalidField = new HandlerInvalidField<String>() {};

    assertThat(handlerInvalidField.handle(new String()), not(nullValue()));
    assertThat(handlerInvalidField.handle(new String())).isInstanceOf(Collection.class);
    assertThat(handlerInvalidField.handle(new String()), empty());
    
  }

  @Test
  public void testSuccessDefaultHandle2() {

    final HandlerInvalidField<String> handlerInvalidField = new HandlerInvalidField<String>() {};

    assertThat(handlerInvalidField.handle(new Object(), new String()), not(nullValue()));
    assertThat(handlerInvalidField.handle(new Object(), new String())).isInstanceOf(Collection.class);
    assertThat(handlerInvalidField.handle(new Object(), new String()), empty());
    
  }

}
