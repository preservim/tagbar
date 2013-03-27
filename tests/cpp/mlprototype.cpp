/* -*- Mode: C++; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* vim:set ts=2 sw=2 sts=2 et cindent: */

  NS_IMETHODIMP
  nsThreadClassInfo::GetClassDescription(
    char **result,
    int foo,
    bool blah
  )
  {
    *result = nsnull;
    return NS_OK;
  }

int foo;
