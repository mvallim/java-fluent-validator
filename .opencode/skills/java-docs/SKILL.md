---
name: java-docs
description: 'Ensure that Java types are documented with Javadoc comments and follow best practices for documentation.'
---

# Javadoc Skill

## 🎯 **Skill Overview**
This skill enables the creation and maintenance of comprehensive Javadoc documentation for Java codebases, ensuring professional code documentation standards.

## 📋 **Skill Details**

### 🔧 **Core Competencies**
- **Javadoc Generation**: Create comprehensive documentation using Javadoc comments
- **Code Documentation**: Document classes, methods, and interfaces with proper tags
- **Standard Compliance**: Follow Javadoc conventions and best practices
- **Tool Integration**: Generate documentation using javadoc tool and IDE integration

### 📝 **Technical Requirements**

#### **Basic Javadoc Structure**
```java
/**
 * Class description
 */
public class MyClass {
    /**
     * Method description
     * @param parameter Description of parameter
     * @return Description of return value
     * @throws Exception Description of exception
     */
    public void myMethod(String parameter) throws Exception {
        // implementation
    }
}
```

#### **Required Javadoc Tags**
- `@param` - Parameter descriptions
- `@return` - Return value description
- `@throws` - Exception descriptions
- `@see` - Related documentation
- `@deprecated` - Deprecation notice

### 🎯 **Performance Indicators**

#### **Mastery Levels**
- **Beginner**: Basic Javadoc comments for public methods
- **Intermediate**: Complete class and method documentation with all required tags
- **Advanced**: Professional documentation with cross-references and examples

#### **Quality Metrics**
- 100% of public classes documented
- 100% of public methods documented
- Proper use of all required Javadoc tags
- Consistent documentation style
- No missing or incomplete documentation

### 🛠️ **Implementation Process**

#### **Step 1: Class Documentation**
```java
/**
 * User management service
 */
public class UserService {
    // class implementation
}
```

#### **Step 2: Method Documentation**
```java
/**
 * Creates a new user account
 * @param username the user's username
 * @param email the user's email address
 * @return the created user object
 * @throws IllegalArgumentException when username or email is null
 * @throws UserCreationException when user creation fails
 */
public User createUser(String username, String email) 
    throws IllegalArgumentException, UserCreationException {
    // implementation
}
```

### 📊 **Skill Validation**

#### **Checklist**
- [ ] All public classes have Javadoc
- [ ] All public methods have Javadoc
- [ ] Required tags are present
- [ ] Documentation is clear and concise
- [ ] Examples are provided when needed
- [ ] Cross-references are included appropriately

#### **Quality Standards**
- Documentation must be written in English
- Use proper grammar and spelling
- Maintain consistent formatting
- Include relevant examples
- Update documentation with code changes

### 📈 **Skill Progression**

#### **Level 1: Basic Documentation**
- Document public classes
- Add basic method documentation
- Use essential tags

#### **Level 2: Complete Documentation**
- Document all public API
- Include parameter and return descriptions
- Add exception documentation

#### **Level 3: Professional Documentation**
- Advanced cross-references
- Code examples
- Comprehensive coverage
- Integration with documentation tools

### 🎯 **Best Practices**

#### **Documentation Standards**
1. **Clarity**: Write clear, concise descriptions
2. **Consistency**: Maintain uniform style throughout
3. **Completeness**: Include all required information
4. **Accuracy**: Keep documentation updated with code changes
5. **Readability**: Use proper formatting and structure

#### **Tools Integration**
- IDE auto-completion support
- Documentation generation tools
- Continuous integration integration
- Version control integration

### 📚 **Resources**

#### **Reference Materials**
- Oracle Javadoc Documentation
- Java Language Specification
- Code style guidelines
- Project documentation standards

#### **Training Resources**
- Javadoc tutorial examples
- Documentation style guides
- Code review checklists
- Best practice workshops

### 🎯 **Expected Outcomes**
- Professional quality code documentation
- Maintainable and readable documentation
- Consistent documentation standards
- Improved code understanding and maintenance
- Better collaboration among development teams

## 🎯 **Skill Assessment**

### **Evaluation Criteria**
- Documentation completeness
- Tag usage accuracy
- Code clarity and quality
- Tool integration effectiveness
- Consistency across codebase

### **Success Metrics**
- 95%+ documentation coverage
- Zero missing required tags
- Positive code review feedback
- Successful tool integration
- Team adoption rate