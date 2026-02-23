# no_lclTranslator
Demonstration of how to translate an appliation without using LCLTranslator

_ Is there an elegant way of dealing with UI strings that are used repeatedly? 
Or can the inspector refer to (resource) strings defined elsewhere?_

- Don't use Default/LCLTranslator
- Declare all strings as "resourcestrings"
- Uncheck the box "Extract translatable strings from a form when saving its LFM file" in the i18n prage of the project settings (to prevent creation of the .lrj files)
- Run the translation manually by calling "TranslateUnitResourceStrings" in the translations unit. 

This way you have full control over the strings entering the pot file. 

As a disadvantage, you must provide code for each form to assign the 
resourcestrings to the captions/hints/texts of the controls.
